(in-package :plant)

(use-package '(#:com.google.base
               #:com.google.flag))

(defparameter *commands* (make-hash-table :test 'equal))


;; TODO improve the options setup
(defmacro defcmd (name help &body body)
  (let ((key (string name)))
    `(progn
       (defun ,name (options)
         ,help
         ,@body)
       (setf (gethash ,key *commands*) #',name))))

(define-condition command-not-found (error)
  ((command :initarg :command))
  (:report (lambda (c s)
             (with-slots (command) c
               (format s "The command \"~a\" is not registered."
                       command)))))

(defun dispatch-command (name options)
  (let ((cmd-fn (gethash (string-upcase name) *commands*)))
    (unless cmd-fn
      (error 'command-not-found :command name))
    (apply cmd-fn (list options))))

(defcmd help
    "Displays each command and it's help string."
  ;; TODO take an option to print the help for a specific command
  (loop :for k :being :the :hash-keys :in *commands*
     do (let ((cmd-fn (gethash k *commands*)))
          (format t "~a~%" (documentation cmd-fn 'function)))))

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
;; or this command line option.
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
(defvar *command-line*
  #+sbcl
  (cdr (uiop:command-line-arguments))
  #+(or clisp clozure)
  (uiop:command-line-arguments))

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

(defun main ()
  (format t "PLANT_HOME=~a~%PLANT_LISP=~a~%" *home* *lisp*)
  (when *rlwrap-p*
      (format t "rlwrap is available.~%"))
  
  ;; Do we need to download quicklisp?
  (unless (uiop:directory-exists-p (merge-pathnames "quicklisp/" plant:*home*))
    (format t "Downloading quicklisp~%")
    (install-quicklisp))
  
  (load (merge-pathnames "quicklisp/setup.lisp" *home*))
  
  (let ((args (parse-command-line *command-line*)))
    (if *dev*
        (progn
          ;; clisp should just hit the toplevel repl from here
          ;; without any push from us.
          ;; sbcl and clozure will exit unless we call the toplevel.
          #+sbcl (sb-impl::toplevel-init)
          #+clozure (ccl:toplevel-loop))
        (progn
          (let ((command (first args))
                (options (rest args)))
            (dispatch-command command options))
          #+clisp (cl-user:quit)))))
