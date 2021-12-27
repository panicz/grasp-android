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
