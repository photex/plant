;; Default project data definitions

(define sbcl-config
  '((#:no-user-init . "--no-userinit")
    (#:load . "--load")
    (#:eval . "--eval")
    (#:save . "'(save-lisp-and-die #P\".plant/~a-~a\" :executable t :purify t)'")))

(define clozure-config
  '((#:no-user-init . "-n")
    (#:load . "-l")
    (#:eval . "-e")
    (#:save . "'(save-application #P\".plant/~a-~a\" :prepend-kernel t :purify t)'")))

(define default-config-values
  `(("sbcl" . ,sbcl-config)
    ("ccl" . ,clozure-config)
    ("ccl64" . ,clozure-config)))
