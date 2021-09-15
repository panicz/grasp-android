(set! %load-path (cons "/data/data/com.termux/files/home/.guile.d" %load-path))

(use-modules (grand scheme))

(define head (make-procedure-with-setter car set-car!))

(define tail (make-procedure-with-setter cdr set-cdr!))

(define-syntax (define-property (property-name object) default)
  (define property-name
    (let* ((override (make-weak-key-hash-table))
	   (getter (lambda (object)
		     (hashq-ref override object default)))
	   (setter (lambda (arg value)
		     (hashq-set! override arg value)))
	   (unsetter (lambda (object)
		       (hashq-remove! override object)))
	   (proc (make-procedure-with-setter getter setter)))
      (with-procedure-properties ((unsetter unsetter)) proc))))

(define-syntax (unset! (procedure object))
  ((procedure-property procedure 'unsetter) object))

(define-syntax (update! variable value)
  (when (isnt variable equal? value)
    (set! variable value)))

(define-property (dotted? cell)
  (not (or (null? (tail cell))
	   (pair? (tail cell)))))

(define-property (pre-head-space cell) "")

(define-property (post-head-space cell)
  (if (and (not (dotted? cell))
	   (null? (tail cell)))
      ""
      " "))

(define-property (pre-tail-space cell) " ")

(define-property (post-tail-space cell) "")

(define-property (null-head-space cell) "")

(define-property (null-tail-space cell) "")

(define (show-pair p)
    (cond ((null? (head p))
	   (write-char #\()
	   (display (null-head-space p))
	   (write-char #\)))
	  (else
	   (show (head p))))
    (display (post-head-space p))
    (cond ((dotted? p)
	   (write-char #\.)
	   (display (pre-tail-space p))
	   (cond ((null? (tail p))
		  (write-char #\()
		  (display (null-tail-space p))
		  (write-char #\)))
		 (else
		  (show (tail p))))
	   (display (post-tail-space p))
	   (write-char #\)))
	  ((null? (tail p))
	   (write-char #\)))
	  (else
	   ;;(assert (pair? (tail p)))
	   (show-pair (tail p)))))

(define (show p)
  (cond
   ((pair? p)
    (write-char #\()
    (display (pre-head-space p))
    (show-pair p))
   (else
    (write p))))

(e.g.
 (let ((object '(1 2 3)))
   (set! (dotted? object) #t)
   (set! (pre-head-space object) " ")
   (set! (post-tail-space object) " ")
   (set! (dotted? (tail object)) #t)
   (set! (dotted? (tail (tail object))) #t)
   (set! (null-tail-space (tail (tail object))) " ")
   (with-output-to-string (lambda () (show object))))
 ===> "( 1 . (2 . (3 . ( ))) )")

(define (separator? c)
  (or (eof-object? c)
      (char-whitespace? c)
      (is c memq '(#\( #\)))))

(define (read-atom-chars-into last-tail)
  (let ((c (read-char)))
    (cond ((separator? c)
	   (unread-char c))
	  (else
	   (set! (tail last-tail) (cons c '()))
	   (read-atom-chars-into (tail last-tail))))))

(define (read-spaces)
  (define (read-spaces-into result)
    (let ((c (read-char)))
      (cond
       ((eof-object? c)
	result)

       ((char-whitespace? c)
	(read-spaces-into (cons c result)))

       (else
	(unread-char c)
	result)
       )))
  (list->string (reverse (read-spaces-into '()))))

(define (read-list)
  (let ((result '())
	(growth-cone '())
	(initial-space (read-spaces)))

    (define (add-element! element following-space)
      (cond ((null? result)
	     (set! result (cons element '()))
	     (set! growth-cone result)
	     (update! (pre-head-space growth-cone)
		      initial-space))
	    (else
	     (set! (tail growth-cone) (cons element '()))
	     (set! growth-cone (tail growth-cone))))
      (update! (post-head-space growth-cone)
	       following-space))
    
    (define (read-next)
      (let* ((c (read-char)))
    
	(cond ((or (eof-object? c) (eq? c #\)))
	       (values result initial-space))

	      ((eq? c #\.)
	       (let* ((spaces (read-spaces))
		      (c (read-char)))
		 (update! (pre-tail-space growth-cone)
			  spaces)
		 (if (eq? c #\()
		     (let ((result* spaces* (read-list)))
		       (when (null? result*)
			 (update! (null-tail-space growth-cone)
				  spaces*))
		       (set! (tail growth-cone) result*))
		     (let ((output (cons c '())))
		       (read-atom-chars-into output)
		       (set! (tail growth-cone)
			     (list->symbol output))))
		 (update! (dotted? growth-cone) #t)
		 (set! initial-space (read-spaces))
		 (update! (post-tail-space growth-cone)
			  initial-space)
		 (read-next)))

	      ((eq? c #\()
	       (let ((result* spaces* (read-list)))
		 (add-element! result* (read-spaces))
		 (if (null? result*)
		     (update! (null-head-space growth-cone)
			      spaces*))
		 (read-next)))
		
	      (else
	       (let ((output (cons c '())))
		 (read-atom-chars-into output)
		 (add-element! (list->symbol output)
			       (read-spaces))
		 (read-next))))))

    (read-next)))

(e.g.
 (let* ((input "( (  ) a  (   b  . (  

  )  ) ) ( y g .   z ) ")
	(parsed (with-input-from-string input read-list))
	(reconstructed (with-output-to-string
			 (lambda ()
			   (show parsed)))))
   (string=? (string-append "("input")")
	     reconstructed))
