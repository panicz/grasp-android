(define-syntax e.g.
  (syntax-rules (===> $bracket-list$ $string$)
    ((_ example)
     (or example
	 (error 'example)))
    
    ((_ example ===> ($bracket-list$ . value))
     (let ((result example))
       (if (equal? result value)
	   result
	   (error '(example ===> value) result))))

    ((_ example ===> ($string$ . value))
     (let ((result example))
       (if (equal? result ($string$ . value))
	   result
	   (error `(example ===> ,($string$ . value)) result))))
    
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
