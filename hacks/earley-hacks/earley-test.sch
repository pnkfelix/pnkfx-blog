(cond ((and (environment-variable? (interaction-environment) 'earley-compute)
            (environment-variable? (interaction-environment) 'ec-back-pointers))
       (eval '(begin (define bp (ec-back-pointers (earley-compute AE 0 "a+a*a"))) (define rc (car (cadr bp)))))))
