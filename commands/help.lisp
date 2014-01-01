(in-package :plant)

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
