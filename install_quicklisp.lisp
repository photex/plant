(require "asdf")
(load "quicklisp.lisp")
(let ((quicklisp-home (merge-pathnames "quicklisp/" (format nil "~a/" (asdf::getenv "PLANT_HOME")))))
  (quicklisp-quickstart:install :path quicklisp-home))


