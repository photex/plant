;; Default project data definitions

(define plant-default-config
  '((#:no-user-init . "--no-userinit")
    (#:load . "--load")
    (#:eval . "--eval")
    (#:save . "'(save-lisp-and-die #P\".plant/~a-~a\" :executable t :purify t)'")))
