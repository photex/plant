(in-package :plant)

(use-package '(#:com.google.base
               #:com.google.flag))

(define-flag *help*
    :default-value nil
    :selector "help"
    :type boolean
    :documentation "Display usage information.")

(define-flag *dev*
    :default-value nil
    :selector "dev"
    :type boolean
    :help "When hacking on plant itself you can use this to swank it up."
    :documentation "Rather than execute plant, run a swank server.")

;;;*plant-home* should be defined when plant is built
;;; but *plant-lisp* should be configurable by the environment.
(defvar *lisp*
  (uiop/os:getenv "PLANT_LISP"))
(defvar *rlwrap-p*
  (= 0 (asdf:run-shell-command "which rlwrap")))

;; due to the need to use '--' under clisp we just remove
;; that item from the command line args when plant was built
;; with another lisp.
(defvar *args*
  #-clisp (cdr (uiop:command-line-arguments))
  #+clisp (uiop:command-line-arguments))

(defun main ()
  (format t "PLANT_HOME=~a~%PLANT_LISP=~a~%" *home* *lisp*)
  (when *rlwrap-p*
      (format t "rlwrap is available.~%"))
  (parse-command-line *args*)
  (if *dev*
      (progn
        ;; clisp should just hit the toplevel repl from here
        ;; without any push from us.
        ;; sbcl will exit unless we call the toplevel
        ;; clozure ???
        #+sbcl (sb-impl::toplevel-init))
      (progn
        #+clisp (cl-user:quit))))
