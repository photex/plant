;;; This file builds the top level plant binary.
;;; It defines the core plant package and various parameters
;;; that will be used by the plant runtime to perform its work.
;;;
;;; TODO This is all heavily geared towards *developing* plant
;;; rather than running it. Our top level function doesn't do
;;; anything other than load plant each time it's run... that's
;;; a no go for the majority of use-cases.
;;; For the time being I'm going to leave this as it is, but what
;;; should really happen is that plant gets loaded and saved with
;;; the binary, and *only* reloaded when using the --dev flag.
;;; This also means that we will parse the command line in two stages.

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
