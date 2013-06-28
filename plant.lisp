(cl:in-package :cl-user)
(defpackage :plant
  (:use :cl)
  (:export #:run))
(cl:in-package :plant)

(defun usage ()
  (format t "plant [new | build | update | help] [options]~%"))

(defun run (options)
  "entry point to the plant system"
  (let ((command (intern (string-upcase (first options)) "KEYWORD"))
        (args (rest options)))
    (case command
      (:new (format t "making a new system~%"))
      (:build (format t "building the system~%"))
      (:update (format t "updating the system~%"))
      (:help (if args
                 (format t "showing the help for this command~%")
                 (usage)))
      (otherwise (usage))))
  (ccl:quit))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (run ccl:*unprocessed-command-line-arguments*))
