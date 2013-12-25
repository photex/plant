(in-package :plant)

(use-package '(#:com.google.base
               #:com.google.flag))

(define-flag *help*
    :default-value nil
    :selector "help"
    :type boolean
    :documentation "Display the available commands, or the specific help for a command.")

(define-flag *dev*
    :default-value nil
    :selector "dev"
    :type boolean
    :help "When hacking on plant itself you can use this to swank it up."
    :documentation "Run the toplevel repl instead of running plant.")

(define-flag *port*
    :default-value 4005
    :selector "port"
    :type (integer 1024 65535)
    :help "Select the port to run swank on. Default is 4005."
    :documentation "Select the swank port. Default is 4005.")

;; *home* should be defined when plant is built
;; but *lisp* should be configurable by the environment
;; or the command line option.
(define-flag *lisp*
    :default-value (uiop/os:getenv "PLANT_LISP")
    :selector "lisp"
    :type string
    :help "Specify the lisp to use. Defaults to the value of the PLANT_LISP env var.")

(define-flag *settings*
    :default-value (merge-pathnames "runtime/settings.lisp" *home*)
    :selector "settings"
    :type string
    :help "For lisps that aren't yet officially supported, you can provide your own settings in order to use it.")

(defvar *rlwrap-p*
  (= 0 (asdf:run-shell-command "which rlwrap")))

;; Clisp and Clozure properly handle the '--' flag to
;; indicate that the remainder of the command line is to
;; be passed to the program. Under SBCL we need to remove
;; the '--' ourselves.
(defvar *args*
  #+sbcl
  (cdr (uiop:command-line-arguments))
  #+(or clisp clozure)
  (uiop:command-line-arguments))

(defun main ()
  (format t "PLANT_HOME=~a~%PLANT_LISP=~a~%" *home* *lisp*)
  (when *rlwrap-p*
      (format t "rlwrap is available.~%"))
  (let ((commands (parse-command-line *args*)))
    (if *dev*
        (progn
          ;; clisp should just hit the toplevel repl from here
          ;; without any push from us.
          ;; sbcl will exit unless we call the toplevel
          ;; clozure ???
          #+sbcl (sb-impl::toplevel-init)
          #+clozure (ccl:toplevel-loop))
        (progn
          (format t "~a~%" commands)
          #+clisp (cl-user:quit)))))
