(ql:quickload :cffi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((current-dir (truename ".")))
    (pushnew current-dir cffi:*foreign-library-directories*)
    (pushnew current-dir asdf:*central-registry*)))
