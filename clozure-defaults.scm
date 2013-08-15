;; Default project data definitions

(define plant-default-config
  '((#:no-user-init . "-n")
    (#:load . "-l")
    (#:eval . "-e")
    (#:save . "'(save-application #P\".plant/~a-~a\" :prepend-kernel t :purify t)'")))
