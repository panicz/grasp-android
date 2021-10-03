(import (srfi :11) (srfi :17))

(define-alias make-weak-key-hash-table java.util.WeakHashMap)
(define-alias Pair gnu.lists.Pair)
(define-alias System java.lang.System)

(define-syntax define-syntax-rule 
  (syntax-rules ()
    ((define-syntax-rule (name . args) substitution)
     (define-syntax name
       (syntax-rules ()
	 ((name . args)
	  substitution))))))

;; we override Pair with Object's default equality and hash functions
;; (TODO: patch the Kawa implementation of Cons)

(define-simple-class cons (Pair)
  ((*init* a d) (invoke-special Pair (this) '*init* a d))
  ((equals object) ::boolean (eq? object this))
  ((hash-code) ::int (System:identity-hash-code this)))

(define head car)

(define tail cdr)
(define (list->symbol l)
  (string->symbol (list->string l)))

(define (with-output-to-string proc)
  (call-with-output-string
    (lambda (port)
      (parameterize ((current-output-port port))
	(proc)))))

(define (with-input-from-string s proc)
  (call-with-input-string s
    (lambda (port)
      (parameterize ((current-input-port port))
	(proc)))))

(define (hashq-set! table::java.util.WeakHashMap key value)
  (table:put key value))

(define (hashq-ref table::java.util.WeakHashMap key default)
  (if (table:contains-key key)
      (table:get key)
      default))

(define-syntax-rule (define-property (property-name object) default)
  (define property-name
    (let* ((override (make-weak-key-hash-table))
	   (getter (lambda (object)
		     (hashq-ref override object default))))
      (set! (setter getter) (lambda (arg value)
			      (hashq-set! override arg value)))
      getter)))

(define-syntax-rule (update! (property object) value)
  (when (not (equal? (property object) value))
    (set! (property object) value)))

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

(define cell-display-properties
  (list
   dotted?
   pre-head-space
   post-head-space
   pre-tail-space
   post-tail-space
   null-head-space
   null-tail-space))

(define (tree-map/preserve properties f l)
  (define (preserve-properties original cell)
    (for-each (lambda (property)
		(update! (property cell)
			 (property original)))
	      properties)
    cell)
  (if (pair? l)
      (preserve-properties
       l (cons (tree-map/preserve properties f (head l))
	       (tree-map/preserve properties f (tail l))))
      (f l)))

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

(define (show-string p)
  (with-output-to-string
    (lambda ()
      (show p))))

(define (separator? c)
  (or (eof-object? c)
      (char-whitespace? c)
      (memq c '(#\( #\)))))

(define (read-atom-chars-into last-tail)
  (let ((c (peek-char)))
    (unless (separator? c)
      (set! (tail last-tail) (cons (read-char) '()))
      (read-atom-chars-into (tail last-tail)))))

(define (read-spaces)
  (define (read-spaces-into result)
    (let ((c (peek-char)))
      (if (or (eof-object? c)
	      (not (char-whitespace? c)))
	  result
	  (read-spaces-into (cons (read-char) result)))))
  (list->string (reverse (read-spaces-into '()))))

(define (read-list)
  (let ((result '())
	(growth-cone '())
	(initial-space (read-spaces)))

    (define (add-element! element following-space)
      (cond ((null? result)
	     (set! result (cons element '()))
	     (set! growth-cone result))
	    (else
	     (set! (tail growth-cone) (cons element '()))
	     (set! growth-cone (tail growth-cone))))
      (update! (post-head-space growth-cone)
	       following-space))
    
    (define (read-next)
      (let* ((c (read-char)))
	
	(cond ((or (eof-object? c) (eq? c #\)))
	       (when (pair? result)
		 (update! (pre-head-space result)
			  initial-space))
	       (values result initial-space))

	      ((eq? c #\.)
	       (let* ((spaces (read-spaces))
		      (c (read-char)))
		 (if (eq? c #\()
		     (let-values (((result* spaces*) (read-list)))
		       (when (null? result*)
			 (update! (null-tail-space growth-cone)
				  spaces*))
		       (set! (tail growth-cone) result*))
		     (let ((output (cons c '())))
		       (read-atom-chars-into output)
		       (set! (tail growth-cone)
			     (list->symbol output))))
		 (update! (pre-tail-space growth-cone)
			  spaces)
		 (update! (dotted? growth-cone) #t)
		 (update! (pre-head-space result)
			  initial-space)
		 (set! initial-space (read-spaces))
		 (update! (post-tail-space growth-cone)
			  initial-space)
		 (read-next)))

	      ((eq? c #\()
	       (let-values (((result* spaces*) (read-list)))
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

(define (parse port)
  (parameterize ((current-input-port port))
    (let-values (((result spaces) (read-list)))
      result)))
