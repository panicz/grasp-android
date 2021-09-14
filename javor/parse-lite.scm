(set! %load-path (cons "/data/data/com.termux/files/home/.guile.d" %load-path))

(use-modules (grand scheme) (grand define-keywords) (rnrs base))

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

(procedure-properties dotted?)

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
    (display (pre-head-space p))
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
	   (assert (pair? (tail p)))
	   (show-pair (tail p)))))

(define (show p)
  (cond
   ((pair? p)
    (write-char #\()
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


;; jak ma dzialac ten nasz algorytm?
;; chodzi o to, zeby wywolywac 'cons' tylko tyle razy, ile trzeba.

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

;; proces wczytywania listy:
;; 1. wczytujemy spacje
;; 2a. jezeli natrafilismy na eof albo nawias zamykajacy, to
;;     zwracamy wczytane elementy oraz ostatnia spacje
;; 2b. jezeli natrafilismy na nawias otwierajacy, to wczytujemy rekurencyjnie
;;      element i spacje za tym elementem
;;     - jezeli w wyniku dostajemy liste pusta, to jako null-head-space
;;     'biezacej komorki' 

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
	(growth-cone '()))
    
    (define (add-element! element preceding-space)
      (cond ((null? result)
	     (set! result (cons element '()))
	     (set! growth-cone result))
	    (else
	     (set! (tail growth-cone) (cons element '()))
	     (set! growth-cone (tail growth-cone))))
      (update! (pre-head-space growth-cone)
	       preceding-space))

    (define (read-next)
      (let* ((spaces (read-spaces))
	     (c (read-char)))
    
	(cond ((or (eof-object? c) (eq? c #\)))
	       (unless (null? result)
		 (update! (post-head-space result) spaces))
	       (values result spaces))

	      #|
	      ((eq? c #\.)
	       (update! (post-head-space growth-cone) spaces)
	       (let* ((spaces (read-spaces))
		      (c (read-char)))
		 (update! (pre-tail-space growth-cone)
			  spaces)
		 (if (eq? c #\()
		     (let ((output (cons c '())))
		       (read-atom-chars-into output)
	       )
|#

	      ((eq? c #\()
	       (let ((result* spaces* (read-list)))
		 (add-element! result* spaces)
		 (if (null? result*)
		     (update! (null-head-space growth-cone)
			      spaces*))
		 (read-next)))
		
	      (else
	       (let ((output (cons c '())))
		 (read-atom-chars-into output)
		 (add-element! (list->symbol output) spaces)
		 (read-next))))))

    (read-next)))

(show
(with-input-from-string "\
x
( a
b (
) )
" read-list)
)
