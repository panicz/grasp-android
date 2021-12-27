(define-syntax e.g.
  (syntax-rules (===>)
    ((_ example)
     (or example
	 (error 'example)))
    ((_ example ===> value)
     (let ((result example))
       (if (equal? result 'value)
	   result
	   (error '(example ===> value) result))))

    ((_ example ===> value ...)
     (call-with-values (lambda () example)
       (lambda results
	 (if (equal? results '(value ...))
	     (apply values results)
	     (error '(example ===> value ...) results)))))
    ))
