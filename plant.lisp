;; Can't use swank if it gets loaded before
;; you save an image in clisp. Have to load it
;; each run instead.
#+clisp(asdf:load-system "swank")

;;;*plant-home* should be defined when plant is built
;;; but *plant-lisp* should be configurable by the environment.
(defvar *plant-lisp*
  (uiop/os:getenv "PLANT_LISP"))
(defvar *rlwrap-p*
  (= 0 (asdf:run-shell-command "which rlwrap")))

(defvar *args* ())

#+sbcl
(defun main ()
  (let ((*args* (cddr sb-ext:*posix-argv*)))
    (%main)))

#+clozure
(defun main ()
  (format t "Clozure support is not implemented yet.~%")
  (quit))

#+clisp
(defun main ()
  (let ((*args* ext:*args*))
    (%main)))

(defun %main ()
  (format t "PLANT_HOME=~a~%PLANT_LISP=~a~%" *plant-home* *plant-lisp*)
  (when *rlwrap-p*
      (format t "rlwrap is available.~%"))
  (swank:create-server :dont-close t)
  ;(quit)
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (main))
