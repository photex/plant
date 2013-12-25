;;; This file builds the top level plant binary.
;;; It defines the core plant package and various parameters
;;; that will be used by the plant runtime to perform its work.

(defpackage #:plant
  (:use #:cl)
  (:export #:*home*
           #:main))

(in-package :plant)

(defvar *home* (uiop/pathname:ensure-directory-pathname
                (uiop/os:getenv "PLANT_HOME")))

(in-package :cl-user)

(defun boot ()
  "Our top level function loads plant.lisp runs plant:main, then exits."
  (let ((plant (merge-pathnames "plant.lisp" plant:*home*)))
    (load plant)
    (plant:main)))

;; TODO This is the legacy method of telling asdf
;; where to find systems. 
(pushnew (merge-pathnames "lib/base/" plant:*home*)
         asdf:*central-registry*)
(pushnew (merge-pathnames "lib/lisp-gflags/" plant:*home*)
         asdf:*central-registry*)

(asdf:load-system "com.google.flag")

;; Do we need to download quicklisp?
(unless (uiop:directory-exists-p (merge-pathnames "quicklisp/" plant:*home*))
  (format t "Downloading quicklisp~%")
  (let ((quickstart (format nil "(quicklisp-quickstart:install :path #P\"~aquicklisp/\")"
                            plant:*home*)))
    (unwind-protect
         (progn 
           (uiop:run-program '("wget" "http://beta.quicklisp.org/quicklisp.lisp"))
           (uiop:run-program
            #+clisp
            (format nil "clisp -x '(load #P\"quicklisp.lisp\") ~a'" quickstart)
            #+sbcl
            (list "sbcl"
                  "--load" "quicklisp.lisp"
                  "--eval" quickstart
                  "--eval" "(quit)")
            :ignore-error-status nil)
      (uiop:delete-file-if-exists "quicklisp.lisp")))))

(load (merge-pathnames "quicklisp/setup.lisp" plant:*home*))

;; By this point we should have everything we need to
;; save the binary.
(let ((plant-bin (merge-pathnames "bin/plant" plant:*home*)))
  (format t "Saving plant binary~%")
  ;; TODO uiop/image:create-image?
  #+sbcl
  (save-lisp-and-die plant-bin :executable t
                     :toplevel #'boot)
  #+clisp
  (saveinitmem plant-bin :executable t :quiet t
               :init-function #'boot)
  #+clozure
  (save-application plant-bin :toplevel-function #'boot
                    :prepend-kernel t)
  (quit))
