(import (srfi :11) (srfi :17))
(import (conversions))
(import (define-property))
(import (screen))
(import (draw))

;; we override Pair with Object's default equality and hash functions
;; (TODO: patch the Kawa implementation of Cons)

(define-simple-class cons (gnu.lists.Pair Tile)
  ((*init* a d) (invoke-special gnu.lists.Pair (this) '*init* a d))
  ((equals object) ::boolean (eq? object (this)))
  ((hash-code) ::int (java.lang.System:identity-hash-code (this)))
  ((draw! screen::Screen)::Extent
   (draw-parenthesized! draw-sequence! (this) screen)))

;; Docelowo bedziemy musieli patchowac Kawe, chociazby po to,
;; zeby takie obiekty, jak Symbol czy FString (IString? MString?)
;; implementowaly Tile.
;;
;; Na razie jednak zrobmy tak, zeby nie uzywac tych typow
;; bezposrednio w edytorze, a zamiast tego korzystac sobie
;; z owijek

(define-simple-class Symbol (Tile)
  (name :: string)
  ((*init* source::string)
   (set! name source))
  ((toString)::String
   (name:toString))
  ((draw! screen::Screen)::Extent
   (screen:draw-atom! name)))

(define head car)

(define tail cdr)

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
			     (Symbol (list->string output)))))
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
		 (add-element! (Symbol (list->string output))
			       (read-spaces))
		 (read-next))))))

    (read-next)))

(define (parse port)
  (parameterize ((current-input-port port))
    (let-values (((result spaces) (read-list)))
      result)))

(define (show-empty-list space)
  (write-char #\()
  (display space)
  (write-char #\)))

(define (show-head p)
  (if (null? (head p))
      (show-empty-list (null-head-space p))
      (show (head p))))

(define (show-dotted-tail p)
  (write-char #\.)
  (display (pre-tail-space p))
  (if (null? (tail p))
      (show-empty-list (null-tail-space p))
      (show (tail p)))
  (display (post-tail-space p)))

(define (show-pair p)
  (display (pre-head-space p))  
  (show-head p)
  (display (post-head-space p))
  (cond ((dotted? p)
	 (show-dotted-tail p))
	((pair? (tail p))
	 (show-pair (tail p)))))

(define (show p)
  (cond
   ((pair? p)
    (write-char #\()
    (show-pair p)
    (write-char #\)))
   (else
    (write p))))

(define (show->string p)
  (with-output-to-string
    (lambda ()
      (show p))))
