(ql:quickload :cffi)

;;; setup.lisp is used to bootstrap some simple things in our saved lisp image.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((current-dir (truename ".")))
    (pushnew current-dir cffi:*foreign-library-directories*)
    (pushnew current-dir asdf:*central-registry*)
    (pushnew (truename "./.plant/deps/") quicklisp-client:*local-project-directories*)))
