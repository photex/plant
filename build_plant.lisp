;;; This file builds the top level plant binary.
;;; It defines the core plant package and various parameters
;;; that will be used by the plant runtime to perform its work.

(defpackage #:plant
  (:use #:cl)
  (:export #:*home*
           #:boot))

(in-package :plant)

(defvar *home*
  (let ((ph-env-val (uiop/os:getenv "PLANT_HOME")))
    #-clisp (truename ph-env-val)
    ;; TODO clisp raises an error when using truename
    ;; on a string which points to a directory that doesn't
    ;; end with a '/'. I need to learn a better way to take
    ;; this env value and create a proper pathname from it.
    #+clisp (handler-case (truename ph-env-val)
              (system::simple-file-error ()
                (truename (format nil "~a/" ph-env-val))))))

;; This is our top level function which loads plant.lisp
;; runs plant main, then exits.
(defun boot ()
  (let ((plant (merge-pathnames "plant.lisp" *home*)))
    (load plant)
    ;; TODO error handling, proper return codes, etc
    (main)
    ;(quit)
    ))

(in-package :cl-user)

;; TODO This is the legacy method of telling asdf
;; where to find systems. 
(pushnew (truename (format nil "~aslime/" plant:*home*))
         asdf:*central-registry*)

;; For some reason I can't create-server if swank
;; is loaded before we save the image with clisp.
#-clisp(asdf:load-system "swank" :verbose nil)

;; By this point we should have everything we need to
;; save the binary.
;; TODO is there an asdf/uiop function for getting argv?
(let* ((args #+sbcl(cdr sb-ext:*posix-argv*)
             #+clisp ext:*args*)
       (plant-bin (merge-pathnames plant:*home* (car args))))
  (format t "Saving plant binary~%")
  ;; TODO uiop/image:create-image?
  #+sbcl
  (save-lisp-and-die plant-bin :executable t
                     :toplevel #'plant:boot)
  #+clisp
  (saveinitmem plant-bin :executable t :quiet t
               :init-function #'plant:boot)
  (quit))
