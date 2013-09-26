((#:sbcl . ((#:no-user-init . "--no-userinit")
            (#:load . "--load")
            (#:eval . "--eval")
            (#:save . "'(save-lisp-and-die #P\"~a\" :executable t :purify t)'")))

 (#:clozure . ((#:no-user-init . "-n")
               (#:load . "-l")
               (#:eval . "-e")
               (#:save . "'(save-application #P\"~a\" :prepend-kernel t :purify t)'"))))
