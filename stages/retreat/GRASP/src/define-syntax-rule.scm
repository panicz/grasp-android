(define-syntax define-syntax-rule
  (syntax-rules ()
    
    ((define-syntax-rule (name . args) substitution)
     (define-syntax name
       (syntax-rules ()
	 ((name . args)
	  substitution))))

    ((define-syntax-rule (name . args) . substitution)
     (define-syntax name
       (syntax-rules ()
	 ((name . args)
	  (begin . substitution)))))
    ))

(define-syntax-rule (define-synonym synonym existing-name)
  (define-syntax-rule (synonym . args)
    (existing-name . args)))
