#!/usr/bin/guile \
--no-auto-compile -e main -s
!#

(use-modules (ice-9 pretty-print))
(use-modules (ice-9 regex))

(define (join-path . args)
  (string-join args "/"))

(define (curpath p)
  "simply join the path fragment with the current working directory"
  (join-path (getcwd) p))

;; globals

(define *plant-lisp* (or (getenv "PLANT_LISP") "sbcl"))
(format #t "PLANT_LISP = ~a~%" *plant-lisp*)

;;; Plant home is where we'll be looking for 
(define *plant-home* (or (getenv "PLANT_HOME") (join-path (getenv "HOME") ".plant")))
(format #t "PLANT_HOME = ~a~%" *plant-home*)

;;; This is the file that holds the specific command line arguments for the
;;; supported lisp implementations.
(define *lisp-settings* '())

(define *plant-project* (curpath "plant-project.scm"))
(define *plant-project-dir* (curpath ".plant"))
(define *deps-dir* (join-path *plant-project-dir* "deps"))

(define *project-data* '())

(define *quicklisp-url* "http://beta.quicklisp.org/quicklisp.lisp")

(define *supported-dvcs* '("git" "hg"))

;; utilities

(define (path-test op p)
  "wraps the test command."
  (equal? 0 (system (string-join
                     (list "test" op p)))))

(define (file? f)
  "Returns true if 'f' is a valid path to a file."
  (path-test "-f" f))

(define (dir? d)
  "Returns true if 'd' is a valid path to a directory."
  (path-test "-d" d))

(define (project-item item-key)
  (assoc-ref *project-data* item-key))

(define (project-name)
  (project-item #:project-name))

(define (project-lisp)
  (join-path *plant-project-dir*
             (format #f "~a-~a" *plant-lisp* (project-name))))

(define (no-user-init)
  (assoc-ref *lisp-settings* #:no-user-init))

(define (load-arg arg)
  (string-join (list (assoc-ref *lisp-settings* #:load) arg)))

(define (eval-arg arg)
  (string-join (list (assoc-ref *lisp-settings* #:eval) arg)))

(define (save)
  (format #f (assoc-ref *lisp-settings* #:save)
          (project-lisp)))

(define (project-quickloads)
  (project-item #:quickloads))

;; functions

(define (save-project-settings)
  (with-output-to-file *plant-project*
    (lambda ()
      (pretty-print *project-data* (current-output-port)))))

(define (load-project-settings)
  (with-input-from-file *plant-project*
    (lambda ()
      (set! *project-data* (read (current-input-port))))))

(define (install-quicklisp)
  (let ((wget-cmd (string-join (list "wget" *quicklisp-url*)))
        (quicklisp-install-cmd (string-join
                                (list *plant-lisp* (no-user-init)
                                      (load-arg "quicklisp.lisp")
                                      (eval-arg "'(quicklisp-quickstart:install :path #P\"~/.plant/quicklisp/\")'")
                                      (eval-arg "'(quit)'")))))
    (system wget-cmd)
    (system quicklisp-install-cmd)
    (delete-file "quicklisp.lisp")))

(define (build-lisp)
  (let* ((quickloads-fmt (string-join (map (lambda (x) (format #f ":~a" x))
                                           (project-quickloads))))
         (quickloads-arg (string-join
                          (list "'(ql:quickload (list" quickloads-fmt "))'"))))
    ;; Create the .plant directory if it doesn't exist
    (unless (dir? *plant-project-dir*)
      (mkdir *plant-project-dir*))

    ;; Create the project deps directory if it doesn't exist
    (let ((deps-dir (join-path *plant-project-dir* "deps")))
      (unless (dir? deps-dir)
        (mkdir deps-dir)))
    
    (system (string-join
             (list *plant-lisp* (no-user-init)
                   (load-arg (join-path *plant-home* "quicklisp" "setup.lisp"))
                   (load-arg (join-path *plant-home* "setup.lisp"))
                   (eval-arg quickloads-arg)
                   (eval-arg (save)))))))

(define (fetch-dependency dep-config)
  (let* ((method (assoc-ref dep-config #:method))
         (url (assoc-ref dep-config #:url))
         (clone? (if (member method *supported-dvcs*) "clone" ""))
         (dep-name (let* ((url-basename (basename url))
                          (match (string-match "\\.git" url-basename)))
                     (if match
                         (regexp-substitute #f match 'pre "" 'post)
                         url-basename)))
         (destination (join-path *deps-dir* dep-name))
         (cmd-line (string-join (list method clone? url destination))))
    (when (not (dir? destination))
      (format #t "Fetching: ~a~%" url)
      (system cmd-line))))

(define (fetch-dependencies)
  (let ((dependencies (assoc-ref *project-data* #:dependencies)))
    (map fetch-dependency dependencies)))

(define (save-quickloads quickloads)
  (set! *project-data*
        (assoc-set! *project-data* #:quickloads
                    (append (assoc-ref *project-data* #:quickloads)
                            quickloads)))
  (save-project-settings))

;; commands

(define (help options)
  (format #t "plant build [<system> <system> <system> ...]~%")
  (format #t "    - 'setup', 'update' are aliases for build.~%")
  (format #t "plant include <git|hg> <url>~%")
  (format #t "plant quickloads <system> [<system> <system> ...]~%")
  (format #t "plant run [--swank [port]]~%")
  (format #t "    - You can also run `plant swank` as a shortcut.~%"))

(define (quickloads options)
  (if (>= (length options) 1)
      (begin
        ;; TODO If something goes wrong then incorrect settings are saved
        (save-quickloads options)
        (build-lisp))
      (help options)))

(define (setup options)
  (save-quickloads options)
  (fetch-dependencies)
  (build-lisp))

(define (run options)
  (let* ((option-count (length options))
         (swank? (equal? "--swank" (when (>= option-count 1)
                                       (car options))))
         (default-port "4005")
         (port (if (and swank? (>= option-count 2))
                   (cadr options)
                   default-port))
         (top-level-options (if swank?
                                (cddr options)
                                options))
         (swank-arg-template "'(swank:create-server :dont-close t :port ~a)'")
         (swank-args (eval-arg (format #f swank-arg-template port)))
         (has-rlwrap? (begin (format #t "Checking for rlwrap... ")
                             (equal? 0 (system "which rlwrap"))))
         (cmd-line (string-join (list (if has-rlwrap? "rlwrap" "")
                                      (project-lisp)
                                      (string-join top-level-options)
                                      (no-user-init)
                                      (if swank? swank-args "")))))
    (format #t "~a~%" cmd-line)
    (unless (file? (project-lisp))
      (setup '()))
    (format #t (if has-rlwrap? "~%" "rlwrap not found in PATH~%~%"))
    (system cmd-line)))

(define (include options)
  (let ((method (car options))
        (url (cadr options)))
    (unless (member method '("git" "hg"))
      (format #t "~%ERROR: Current only git and hg are supported for this operation.")
      (exit 5))
    (let ((dep-config (acons #:method method (acons #:url url '())))
          (dependencies (assoc-ref *project-data* #:dependencies)))
      
      (set! *project-data*
            (assoc-set! *project-data* #:dependencies
                        (cons dep-config dependencies)))
      
      (save-project-settings)
      (fetch-dependencies))))

;; entry point

(define (main args)
  ;; we need at least a command
  (when (< (length args) 2)
    (help '())
    (exit 255))

  ;; Load the settings for the requested lisp
  (let* ((lisp-settings-file (join-path *plant-home* "lisp-settings.scm"))
         (settings-data (with-input-from-file lisp-settings-file (lambda () (read (current-input-port)))))
         (aliases (map (lambda (config)
                         (list (assoc-ref (cdr config) #:aliases)
                               (car config))) settings-data))
         (desired-lisp #f))
    (map (lambda (alias) (when (member *plant-lisp* (car alias))
                           (set! desired-lisp (cadr alias)))) aliases)
    (if (not desired-lisp)
      (begin
        (help '())
        (format #t "~%ERROR: ~a is not currently supported by plant.~%" *plant-lisp*)
        (exit 2))
      (begin
        (format #t "Using settings for ~a~%" desired-lisp)
        (set! *lisp-settings* (assoc-ref (assoc-ref settings-data desired-lisp) #:settings)))))

  (unless *lisp-settings*
    (format #t "~%ERROR: Unable to load settings for the current lisp.~%")
    (exit 3))

  ;; are we setting up a new project or loading a previously created one?
  (if (not (file? *plant-project*))
      ;; Initialize the project data
      (begin
        (format #t "Unable to locate a project file (~a) creating a new one.~%" *plant-project*)
        (let* ((project-defaults-file (join-path *plant-home* "project-defaults.scm"))
               (project-name (basename (getcwd)))
               (lisp-settings (with-input-from-file (join-path *plant-home* "lisp-settings.scm")
                                (lambda () (read (current-input-port))))))
          ;; Load the defaults and add/update values for the current project
          (set! *project-data* (with-input-from-file project-defaults-file
                                 (lambda () (read (current-input-port)))))
          (set! *project-data* (assoc-set! *project-data* #:project-name project-name)))
        (save-project-settings))
      ;; else load the existing project data and bind it
      (load-project-settings))

  (unless (dir? (join-path (getenv "HOME") ".plant" "quicklisp"))
    (install-quicklisp))

  (unless (defined? '*project-data*)
    (format #t "~%ERROR: Unable to load or create project data!~%")
    (exit 3))
  
  (let* ((command (cadr args))
         (options (cddr args)))
    ;; dispatch further work to the individual command handlers
    (cond ((equal? command "run") (run options))
          ((equal? command "swank") (run (append '("--swank") options)))
          ((equal? command "quickloads") (quickloads options))
          ((equal? command "include") (include options))
          ((equal? command "setup") (setup options))
          ((equal? command "update") (setup options))
          ((equal? command "build") (setup options))
          ((equal? command "help") (help options))
          (#t (begin
                (help options)
                (format #t "~%ERROR: Unknown command ~a~%" command))))))
