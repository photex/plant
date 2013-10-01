(ql:quickload '(:getopt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *default-quickloads* '("swank" "alexandria"))
(defvar *plant-project* #P"")
(defvar *plant-dir* #P"")
(defvar *project-data* '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun help (&optional cmd)
  (format t "Here goes the help!~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main (args)
  ;; we need at least a command
  (when (< (length args) 1)
    (help)
    (sys:exit 255))

  (sys:exit 0))

;; (main (cdr ext:*command-args*))
