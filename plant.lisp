(in-package :plant)

(use-package '(#:com.google.base
               #:com.google.flag))

(defparameter *commands* (make-hash-table :test 'equal))
(defparameter *pre-hooks* (make-hash-table :test 'equal))
(defparameter *post-hooks* (make-hash-table :test 'equal))

(defvar *project* nil)
(defvar *project-directory* nil)
(defvar *options* nil)

(defmacro defcmd (name help &body body)
  (let ((key (string name)))
    `(progn
       (defun ,name ()
         ,help
         ,@body)
       (setf (gethash ,key *commands*) #',name)
       (setf (gethash ,key *pre-hooks*) (make-hash-table :test 'equal))
       (setf (gethash ,key *post-hooks*) (make-hash-table :test 'equal)))))

(defmacro defalias (name command)
  (let* ((key (string name))
         (cmd-name (string command))
         (cmd-help (format nil "Alias for ~a" (string cmd-name))))
    `(progn
       (defun ,name ()
         ,cmd-help
         (dispatch-command ,cmd-name))
       (setf (gethash ,key *commands*) #',name))))

(defmacro defhook (name step command &body body)
  (let* ((key (string name))
         (hook-hash (gethash (string command)
                             (case step
                               (pre *pre-hooks*)
                               (post *post-hooks*)))))
    (unless hook-hash
      (error "When defining a hook, the value of step must be pre or post."))
    `(progn
       (defun ,name ()
         ,@body)
       (setf (gethash ,key ,hook-hash) #',name))))

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

(defun get-command-hooks (name)
  (let ((key (string-upcase name)))
    (values (gethash key *pre-hooks*)
            (gethash key *post-hooks*))))

(defun call-hooks (table)
  (when table
    (loop :for fn :being :the :hash-values :in table
       :do (funcall fn))))

(defun dispatch-command (name)
  (multiple-value-bind (pre-hooks post-hooks) (get-command-hooks name)
    (call-hooks pre-hooks)
    (funcall (get-command name))
    (call-hooks post-hooks)))

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
  (if *options*
      ;; help for a specific command
      (let* ((cmd-name (string-upcase (first *options*)))
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
    :default-value (or (uiop/os:getenv "PLANT_LISP")
                       #+clisp "clisp"
                       #+sbcl "sbcl"
                       #+clozure "ccl")
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

(defun load-files-from (dir)
  (let ((files (uiop/filesystem:directory-files dir)))
    (loop :for file :in files
       :if (string= "lisp" (pathname-type file))
       :do (load file))))

(defun current-project ()
  (unless *project*
    (let* ((cwd (uiop/os:getcwd))
           (project-data (merge-pathnames "project.lisp" cwd)))
      (when (uiop/filesystem:file-exists-p project-data)
        (with-open-file (f project-data)
          (setf *project* (read f))))))
  *project*)

;;; main entry point for the plant application

(defun main (args)
  ;; load local command definitions
  (load-files-from (merge-pathnames "commands/" *home*))

  ;; load user command definitions
  (load-files-from (merge-pathnames ".config/plant/commands/" (user-homedir-pathname)))

  ;; dispatch to the requested command
  (let ((command (first args))
        (*options* (rest args)))
    (handler-case
        (dispatch-command command)
      (command-not-found ()
        (progn
          (dispatch-command "help")
          (format t "ERROR: \"~a\" is not an available command.~%" command))))))
