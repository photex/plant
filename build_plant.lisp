
(defvar *plant-home*
  (let ((ph-env-val #+clisp(ext:getenv "PLANT_HOME")
                    #+sbcl(sb-ext:posix-getenv "PLANT_HOME")))
    (handler-case (truename ph-env-val)
      #+clisp
      (system::simple-file-error ()
        (truename (format nil "~a/" ph-env-val))))))

(format t "Loading ASDF~%")
(load (merge-pathnames *plant-home* "asdf.lisp"))
(pushnew (truename (format nil "~aslime/" *plant-home*))
         asdf:*central-registry*)
;; For some reason I can't create-server if swank
;; is loaded before we save the image. 
#-clisp(asdf:load-system "swank")
(let* ((args #+sbcl(cdr sb-ext:*posix-argv*)
             #+clisp ext:*args*)
       (plant-bin (merge-pathnames *plant-home* (car args))))
  (format t "Saving plant binary~%")
  #+sbcl
  (save-lisp-and-die plant-bin :executable t)
  #+clisp
  (saveinitmem plant-bin :executable t :quiet t)
  (quit))
