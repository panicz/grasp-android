(import (srfi :11) (srfi :17))
(import (class java.util (WeakHashMap make-weak-key-hash-table)))
(import (class gnu.lists Pair))
(import (class java.lang System))

(define-syntax define-syntax-rule 
  (syntax-rules ()
    ((define-syntax-rule (name . args) substitution)
     (define-syntax name
       (syntax-rules ()
	 ((name . args)
	  substitution))))))

;; we override Pair with Object's default equality and hash functions

(define-simple-class cons (Pair)
  ((*init* a d) (invoke-special Pair (this) '*init* a d))
  ((equals object) ::boolean (eq? object this))
  ((hash-code) ::int (System:identity-hash-code this)))


(define-syntax infix/postfix
  (syntax-rules ()
    
    ((infix/postfix x somewhat?)
     (somewhat? x))

    ((infix/postfix left related-to? right)
     (related-to? left right))

    ((infix/postfix left related-to? right . likewise)
     (let ((right* right))
       (and (infix/postfix left related-to? right*)
	    (infix/postfix right* . likewise))))))

(define-syntax extract-_
  (syntax-rules (_ is isnt quote
 		   quasiquote unquote
		   unquote-splicing)
    ;; ok, it's a bit rough, so it requires an explanation.
    ;; the macro operates on sequences of triples
    ;;
    ;;   (<remaining-expr> <arg-list> <processed-expr>) +
    ;;
    ;; where <remaining-expr> is being systematically
    ;; rewritten to <processed-expr>. When the _ symbol
    ;; is encountered, it is replaced with a fresh "arg"
    ;; symbol, which is appended to both <arg-list>
    ;; and <processed-expr>.
    ;;
    ;; The goal is to create a lambda where each
    ;; consecutive _ is treated as a new argument
    ;; -- unless there are no _s: then we do not
    ;; create a lambda, but a plain expression.
    ;;
    ;; The nested "is" and "isnt" operators are treated
    ;; specially, in that the _s within those operators are
    ;; not extracted.
    ;;
    ;; Similarly, the _ isn't extracted from quoted forms,
    ;; and is only extracted from quasi-quoted forms if
    ;; it appears on unquoted positions.

    ;; The support for quasiquote modifies the tuples
    ;; to have the form
    ;;
    ;;   (<remaining-expr> <arg-list> <processed-expr> . qq*) +
    ;;
    ;; where qq* is a sequence of objects that expresses
    ;; the nesting level of the 'quasiquote' operator
    ;; (i.e. quasiquote inside quasiquote etc.)

    ;; The macro consists of the following cases:
    
    ;; fin case with no _s
    ((extract-_ fin (() () body))
     (fin (infix/postfix . body)))

    ;; fin case with some _s -- generate a lambda
    ((extract-_ fin (() args body))
     (lambda args (fin (infix/postfix . body))))

    ;; treat 'is' and 'isnt' operators specially and
    ;; don't touch their _s
    ((extract-_ fin (((is . t) . rest) args (body ...)) . *)
     (extract-_ fin (rest args (body ... (is . t))) . *))

    ((extract-_ fin (((isnt . t) . rest) args (body ...)) . *)
     (extract-_ fin (rest args (body ... (isnt . t))) . *))

    ;; same with 'quote'
    ((extract-_ fin (('literal . rest) args (body ...)) . *)
     (extract-_ fin (rest args (body ... 'literal)) . *))

    ;; when 'quasiquote' is encountered, we increase the
    ;; level of quasiquotation (the length of the qq* sequence)
    ((extract-_ fin
		(((quasiquote x) . rest) args body . qq*) . *)
     (extract-_ fin
		((x) () (quasiquote) qq . qq*)
		(rest args body) . *))

    ;; on the other hand, for 'unquote' and
    ;; 'unquote-splicing', we decrease the nesting level
    ;; (i.e. we consume one element from the qq* sequence)
    ((extract-_ fin
		(((unquote x) . rest) args body qq . qq*) . *)
     (extract-_ fin
		((x) () (unquote) . qq*)
		(rest args body qq . qq*) . *))

    ((extract-_ fin
		(((unquote-splicing x) . rest) args body
		 qq . qq*) . *)
     (extract-_ fin
		((x) () (unquote-splicing) . qq*)
		(rest args body qq . qq*) . *))

    ;; push/unnest nested expression for processing
    ((extract-_ fin (((h . t) . rest) args body . qq) . *)
     (extract-_ fin ((h . t) () () . qq)
		(rest args body . qq) . *))

    ;; unquote in the tail position
    ((extract-_ fin
		((unquote x) args (body ...) qq . qq*) . *)
     (extract-_ fin
		((x) args (body ... unquote) . qq*) . *))
    
    ;; generate a new arg for the _ in the head position
    ((extract-_ fin ((_ . rest) (args ...) (body ...)) . *)
     (extract-_ fin (rest (args ... arg) (body ... arg)) . *))

    ;; rewrite the term in the head position to the back
    ;; of the processed terms
    ((extract-_ fin ((term . rest) args (body ...) . qq) . *)
     (extract-_ fin (rest args (body ... term) . qq) . *))

    ;; _ in the tail position
    ((extract-_ fin
		(_ (args ...) (body ...) . qq)
		(rest (args+ ...) (body+ ...) . qq+) . *)
     (extract-_ fin
		(rest (args+ ... args ... arg)
		      (body+ ... (body ... . arg)) . qq+) . *))

    ;; pop/nest back processed expression
    ;; ('last' is an atom; most likely (), but can also
    ;; be some value, e.g. in the case of assoc list literals)
    ((extract-_ fin
		(last (args ...) (body ...) . qq)
		(rest (args+ ...) (body+ ...) . qq+) . *)
     (extract-_ fin (rest (args+ ... args ...)
			  (body+ ... (body ... . last))
			  . qq+) . *))
    ))

(define-syntax-rule (identity-syntax form)
  form)

(define-syntax-rule (is . something)
  (extract-_ identity-syntax (something () ())))

(define-syntax-rule (isnt . something)
  (extract-_ not (something () ())))

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
  (when (isnt (property object) equal? value)
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

(let ((object '(1 2 3)))
  (set! (dotted? object) #t)
  (set! (pre-head-space object) " ")
  (set! (post-tail-space object) " ")
  (set! (dotted? (tail object)) #t)
  (set! (dotted? (tail (tail object))) #t)
  (set! (null-tail-space (tail (tail object))) " ")
  (show object)
  (newline)
  (newline))

(define (separator? c)
  (or (eof-object? c)
      (char-whitespace? c)
      (is c memq '(#\( #\)))))

(define (read-atom-chars-into last-tail)
  (let ((c (peek-char)))
    (unless (separator? c)
      (set! (tail last-tail) (cons (read-char) '()))
      (read-atom-chars-into (tail last-tail)))))

(define (read-spaces)
  (define (read-spaces-into result)
    (let ((c (peek-char)))
      (if (or (eof-object? c)
	      (isnt c char-whitespace?))
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

(define (read-all)
  (let-values ((parsed _) (read-list))
    parsed))

  
