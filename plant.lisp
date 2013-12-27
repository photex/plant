(in-package :plant)

(use-package '(#:com.google.base
               #:com.google.flag))

(defparameter *commands* (make-hash-table :test 'equal))

(defmacro defcmd (name help &body body)
  (let ((key (string name)))
    `(progn
       (defun ,name (options)
         ,help
         ,@body)
       (setf (gethash ,key *commands*) #',name))))

(defmacro defalias (name cmd-fn)
  (let ((key (string name))
        (cmd-help (format nil "Alias for ~a"(string cmd-fn))))
    `(progn
       (defun ,name (options)
         ,cmd-help
         (,cmd-fn options))
       (setf (gethash ,key *commands*) #',name))))

(define-condition command-not-found (error)
  ((command :initarg :command))
  (:report (lambda (c s)
             (with-slots (command) c
               (format s "The command \"~a\" is not registered."
                       command)))))

(defun get-command (name)
  (let ((cmd-fn (gethash (string-upcase name) *commands*)))
    (unless cmd-fn
      (error 'command-not-found :command name))
    cmd-fn))

(defun dispatch-command (name options)
  (apply (get-command name) (list options)))

(defun display-help (name cmd-fn)
  (format t "~t~a: ~a~%" name (documentation cmd-fn 'function)))

(defcmd help
    "Displays each command and it's help string, or print the help for a specific command only."
  (format t "~%Plant~%Usage: plant <flags> <command> <options>~%")

  ;; Flag summary
  (format t "~%Available flags:~%")
  (loop :for (flag-name . flag) :in com.google.flag::*registered-flags*
     :do (let ((flag-help (com.google.flag::help flag)))
           (format t "~t--~a: ~a~%" flag-name flag-help)))
  
  ;; Command summary
  (if options
      ;; help for a specific command
      (let* ((cmd-name (string-upcase (first options)))
             (cmd-fn (get-command cmd-name)))
        (format t "~%")
        (display-help cmd-name cmd-fn))

      ;; display all help messages
      (progn
        (format t "~%Available commands: ~%")
        (loop :for cmd-name :being :the :hash-keys :in *commands*
           do (let ((cmd-fn (gethash cmd-name *commands*)))
                (display-help cmd-name cmd-fn)))))
  
  (format t "~%"))

;; clear out any previously registered flags
;; since loading the file multiple times would otherwise
;; create duplicate entries.
;; TODO when attempting that in the REPL I got an error
;; so I need to figure out why this exhibits different
;; behavior when just loading the file.
(setf com.google.flag::*registered-flags* ())

(define-flag *port*
    :default-value 4005
    :selector "port"
    :type (integer 1024 65535)
    :help "Used by the 'run' command, this sets the port to run swank on. Default is 4005."
    :documentation "Select the swank port. Default is 4005.")

;; *home* should be defined when plant is built
;; but *lisp* should be configurable by the environment
;; or this command line option.
(define-flag *lisp*
    :default-value (uiop/os:getenv "PLANT_LISP")
    :selector "lisp"
    :type string
    :help "Specify the lisp to use. Defaults to the value of the PLANT_LISP env var.")

(define-flag *settings*
    ;; TODO create a pathname parser so that we can use pathnames
    ;; instead of strings here.
    ;:default-value (merge-pathnames "runtime/settings.lisp" *home*)
    :default-value (format nil "~aruntime/settings.lisp" *home*)
    :selector "settings"
    :type string
    :help "For lisps that aren't yet officially supported, you can provide your own settings in order to use it.")

(defvar *rlwrap-p*
  (= 0 (asdf:run-shell-command "which rlwrap")))

(defun install-quicklisp ()
  "Download quicklisp and creates a plant internal quicklisp install."
  (let ((quickstart (format nil "(quicklisp-quickstart:install :path #P\"~aquicklisp/\")"
                            plant:*home*)))
    (unwind-protect
         (progn 
           (uiop:run-program '("wget" "http://beta.quicklisp.org/quicklisp.lisp"))
           (uiop:run-program
            #+clisp
            (format nil "clisp -x '(load #P\"quicklisp.lisp\") ~a'" quickstart)
            #-clisp
            (list #+sbcl "sbcl"
                  #+clozure "ccl"
                  "--load" "quicklisp.lisp"
                  "--eval" quickstart
                  "--eval" "(quit)")
            :ignore-error-status nil)
           (uiop:delete-file-if-exists "quicklisp.lisp")))))

;;; main entry point for the plant application

(defun main (args)
  ;; Do we need to download quicklisp?
  (unless (uiop:directory-exists-p (merge-pathnames "quicklisp/" plant:*home*))
    (format t "Downloading quicklisp~%")
    (install-quicklisp))
  (let ((command (first args))
        (options (rest args)))
    (format t "COMMAND: ~a, OPTIONS: ~a~%" command options)
    (handler-case
        (dispatch-command command options)
      (command-not-found ()
        (progn
          (dispatch-command "help" ())
          (format t "ERROR: \"~a\" is not an available command.~%" command))))))
