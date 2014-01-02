(in-package :plant)


;;; The "new" command employs quickproject in order to
;;; automate system creation.


(defcmd new
    "Create a new system using quickproject. Takes the name of the system as an option."
  (print *options*))
