(in-package :plant)

(defcmd init
    "Initialize a new project in the current directory or the directory specified"
  (unless (current-project)
    (let* ((target-dir (first *options*))
           (project-root (if target-dir
                             (let ((d (uiop/pathname:ensure-directory-pathname target-dir)))
                               (if (uiop/pathname:absolute-pathname-p d) d
                                   (merge-pathnames d (uiop/os:getcwd))))
                             (uiop/os:getcwd)))
           (project-path (merge-pathnames "project.lisp" project-root))
           (project-data (with-open-file (f (merge-pathnames "runtime/project-defaults.lisp" *home*))
                           (read f))))
      (ensure-directories-exist project-path)
      ;; TODO is this honestly the only way to extract the name of a directory in a pathname?
      (setf (getf project-data :name) (first (last (pathname-directory project-path))))
      (with-open-file (file project-path :direction :output :if-exists :supersede)
        (write project-data :stream file))
      (setf *project* project-data)
      (setf *project-directory* project-root))))

(defhook foo pre init
  (print "PRE INIT"))
(defhook foo post init
  (print "POST INIT"))
