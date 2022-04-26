(import (define-property))
(import (srfi :11) (srfi :17))
(import (conversions))
(import (functions))
(import (primitive))
(import (infix))
(import (examples))
(import (indexable))
(import (space))
(import (tile))
(import (symbol))


(define (separator? c)::boolean
  (or (eof-object? c)
      (char-whitespace? c)
      (memq c '(#\( #\)))))

(define (read-atom-chars-into last-tail::pair)::void
  (let ((c (peek-char)))
    (unless (separator? c)
      (set! (tail last-tail) (cons (read-char) '()))
      (read-atom-chars-into (tail last-tail)))))

(define (read-spaces)
  (let ((result (Space fragments: (cons 0 '()))))
    (define (read-spaces-into pair)
      (let ((c (peek-char)))
	(if (or (eof-object? c)
		(isnt c char-whitespace?))
	    result	    
	    (match (read-char)
	      (#\newline
	       (set! (tail pair)
		 (cons 0 (tail pair)))
	       (read-spaces-into (tail pair)))
	      (#\space
	       (set! (head pair)
		 (+ (head pair) 1))
	       (read-spaces-into pair))
	      (_
	       (read-spaces-into pair))))))
    (read-spaces-into result:fragments)))

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
      (let ((c (read-char)))
	
	(cond
         ((or (eof-object? c) (eq? c #\)))
          (when (pair? result)                 
            (update! (pre-head-space result)
                     initial-space))
          (values result initial-space))

         ((eq? c #\.)
          (let* ((post-dot-spaces (read-spaces))
                 (c (read-char)))
            (cond
             ((eq? c #\()
              (let-values (((result* spaces*) (read-list)))
                (when (null? result*)
                  (update! (null-tail-space growth-cone)
                           spaces*))
                (set! (tail growth-cone) result*)))
             (else ;;presumably a symbol
              (let ((output (cons c '())))
                (read-atom-chars-into output)
                (set! (tail growth-cone)
                      (Symbol (list->string output))))))
            (update! (dotted? growth-cone) #t)
            (update! (pre-tail-space growth-cone)
		     post-dot-spaces)
            (update! (post-tail-space growth-cone)
                     (read-spaces))
            (read-next)))

         ((eq? c #\()
          (let-values (((result* spaces*) (read-list)))
            (add-element! result* (read-spaces))
            (when (null? result*)
              (update! (null-head-space growth-cone)
                       spaces*))
            (read-next)))
         
         (else
          (let ((output (cons c '())))
            (read-atom-chars-into output)
            (add-element! (Symbol (list->string output))
                          (read-spaces))
            (read-next))))))

    (read-next)))

(define (parse #!optional (port (current-input-port)))::list
  (parameterize ((current-input-port port))
    (let-values (((result spaces) (read-list)))
      result)))

(define (parse-string s::string)::list
  (call-with-input-string s parse))

(define (print-space space::Space
		     #!optional (port (current-output-port)))
  #;(write space:fragments port)
  (space:print port))

(define (show-empty-list space)::void
  (write-char #\()
  (print-space space)
  (write-char #\)))

(define (show-head p::pair)::void
  (if (null? (head p))
      (show-empty-list (null-head-space p))
      (show (head p))))

(define (show-dotted-tail p::pair)::void
  (write-char #\.)
  (print-space (pre-tail-space p))
  (if (null? (tail p))
      (show-empty-list (null-tail-space p))
      (show (tail p)))
  (print-space (post-tail-space p)))

(define (show-pair p::pair)::void
  (show-head p)
  (print-space (post-head-space p))
  (cond ((dotted? p)
	 (show-dotted-tail p))
	((pair? (tail p))
	 (show-pair (tail p)))))

(define (show p)::void
  (cond
   ((pair? p)
    (write-char #\()
    (print-space (pre-head-space p))
    (show-pair p)
    (write-char #\)))
   (else
    (write p))))

(define (show->string p)::string
  (with-output-to-string
    (lambda ()
      (show p))))
