;;; This file builds the top level plant binary.
;;; It defines the core plant package and various parameters
;;; that will be used by the plant runtime to perform its work.

(defpackage #:plant
  (:use #:cl)
  (:export #:*home*
           #:main
           #:defcmd
           #:defalias
           #:defhook
           #:defproject))

(in-package :plant)

(defvar *home* (uiop/pathname:ensure-directory-pathname
                (uiop/os:getenv "PLANT_HOME")))

(in-package :cl-user)

(use-package :plant)

;; TODO This is the legacy method of telling asdf
;; where to find systems. 
(pushnew (merge-pathnames "lib/base/" plant:*home*)
         asdf:*central-registry*)
(pushnew (merge-pathnames "lib/lisp-gflags/" plant:*home*)
         asdf:*central-registry*)
(pushnew (merge-pathnames "lib/slime/" plant:*home*)
         asdf:*central-registry*)

(asdf:load-system "com.google.flag")
;; clisp and swank are having troubles. You'll have to manually load it.
#-clisp (asdf:load-system "swank")

;; application entry point
(let ((plant (merge-pathnames "plant.lisp" plant:*home*)))
  (load plant)

  (com.google.flag:define-flag *dev*
      :default-value nil
      :selector "dev"
      :type boolean
      :help "When hacking on plant itself you can use this to invoke the toplevel REPL in the plant binary."
      :documentation "Run the toplevel repl instead of running plant.")
  
  (defun boot ()
    (unwind-protect
         ;; Clisp and Clozure properly handle the '--' flag to
         ;; indicate that the remainder of the command line is to
         ;; be passed to the program. Under SBCL we need to remove
         ;; the '--' ourselves.
         (let ((args (com.google.flag:parse-command-line
                      #+sbcl (cdr (uiop:command-line-arguments))
                      #+(or clisp clozure) (uiop:command-line-arguments))))
           (if *dev*
               ;; hack on plant
               (progn
                 ;; clisp and swank are having troubles. You'll have to manually load it.
                 #-clisp (swank:create-server)
                 ;; clisp should just hit the toplevel repl from here
                 ;; without any push from us.
                 ;; sbcl and clozure will exit unless we call the toplevel.
                 #+sbcl (sb-impl::toplevel-init)
                 #+clozure (ccl:toplevel-loop))
               ;; run plant
               (plant:main args)))
      #+sbcl (sb-ext:quit)
      #+clisp (quit)
      #+clozure (ccl:quit))))

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
