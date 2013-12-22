;; Can't use swank if it gets loaded before
;; you save an image in clisp. Have to load it
;; each run instead.
#+clisp(asdf:load-system "swank" :verbose nil)

(in-package :plant)

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
  ;(quit)
  )
