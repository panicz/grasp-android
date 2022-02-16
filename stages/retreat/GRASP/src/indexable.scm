(import (define-interface))
(import (define-type))
(import (define-property))
(import (match))
(import (infix))
(import (assert))
(import (for))
(import (string-building))

;; Each tile can choose whatever it pleases to be its index
;; (except #!null, for the reason explained below)
;; For built-in types (boxes, combinators, atoms) indices are
;; typically either integers or characters or symbols.
;;
;; The special value #!null means the absence of an index

(define-alias Index java.lang.Object)

(define-alias Indexable* java.lang.Object)

(define-type (Space fragments: pair)
  implementing StringBuilding
  with
  ((buildString out::StringBuilder)::StringBuilder
   (let process ((input fragments))
     (match input
       (`(,first . ,rest)
	(for n from 0 below first
	     (out:append #\space))
	(match rest
	  ('()
	   out)
	  (`(,_ . ,_)
	   (process rest)))))))
  ((toString)::String
   (invoke (buildString (StringBuilder)) 'toString)))

(define head car)

(define tail cdr)

(define-property+ (dotted? cell)
  (not (or (null? (tail cell))
	   (pair? (tail cell)))))

(define-property+ (pre-head-space cell) "")

(define-property+ (post-head-space cell)
  (if (and (not (dotted? cell))
	   (null? (tail cell)))
      ""
      " "))

(define-property+ (pre-tail-space cell) " ")

(define-property+ (post-tail-space cell) "")

(define-property+ (null-head-space cell) "")

(define-property+ (null-tail-space cell) "")

(define-property (head-tail-separator cell)
  #!null)

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

(define-constant final-part?::parameter[boolean]
  (make-parameter #f))

(define (cell-index cell::pair index::int)
  (assert (is index >= 0))
  (cond ((= index 0)
         (pre-head-space cell)) ;; trzeba jakos rzutowac do Tile
        ((= index 1)
         (let ((target (car cell)))
           (if (and (null? target) (not (final-part?)))
               (null-head-space cell)
               target)))
        ((= index 2)
         (post-head-space cell)) ;; jak wyzej
        ((dotted? cell)
         (cond ((= index 3)
                (head-tail-separator cell))
               ((= index 4)
                (pre-tail-space cell)) ;; jakos rzutowac do Tile?
               ((= index 5)
                (let ((target (cdr cell)))
                  (if (and (null? target) (not (final-part?)))
                      (null-tail-space cell)
                      target)))
               ((= index 6)
                (post-tail-space cell))))
        (else
         (cell-index (cdr cell) (- index 2)))))

(define (last-cell-index cell::pair #!optional (initial::int 2))::int
  (cond ((dotted? cell)
         (+ initial 4))
        ((pair? (tail cell))
         (last-cell-index (tail cell) (+ initial 2)))
        (else
         initial)))

(define-interface Indexable ()
  (has-children?)::boolean
  
  (part-at index::Index)::Indexable*
  
  (first-index)::Index
  (last-index)::Index
  
  (next-index index::Index)::Index
  (previous-index index::Index)::Index
)

(define (has-children? object)
  (cond ((Indexable? object)
	 (invoke (as Indexable object) 'has-children?))

	((pair? object)
	 #t)
	
	(else
	 #f)))

(define (part-at index::Index object)::Indexable*
  (cond ((Indexable? object)
	 (invoke (as Indexable object) 'part-at index))

	((pair? object)
	 (if (or (eq? index #\() (eq? index #\)))
	     object
	     (cell-index object (as int index))))
	 
	(else
	 (error "Don't know how to extract "index" from "object))))

(define (first-index object)
  (cond ((Indexable? object)
	 (invoke (as Indexable object) 'first-index))

	((string? object)
	 0)

	((pair? object)
	 #\()
	
	(else
	 (error "Don't know how to obtain first index from "object))))

(define (last-index object)
  (cond ((Indexable? object)
	 (invoke (as Indexable object) 'last-index))

	((string? object)
	 (string-length (as string object)))

	((pair? object)
	 #\))
	
	(else
	 (error "Don't know how to obtain last index from "object))))

(define (next-index index::Index object)::Index
  (cond ((Indexable? object)
	 (invoke (as Indexable object) 'next-index index))

	((string? object)
	 (min (string-length object) (+ index 1)))

	((pair? object)
	 (match index
	   (#\( 0)
	   (#\) #\))
	   (,@(is _ < (last-cell-index object))
	    (+ index 1))
	   (_
	    #\))))
	
	(else
	 (error "Don't know how to obtain next index to "index
		" in "object))))

(define (previous-index index::Index object)::Index
  (cond ((Indexable? object)
	 (invoke (as Indexable object) 'previous-index index))

	((string? object)
	 (max 0 (- index 1)))

	((pair? object)
	 (match index
	   (0 #\()
	   (#\) (last-cell-index object))
	   (#\( #\()
	   (_ (- index 1))))
	
	(else
	 (error "Don't know how to obtain previous index to "index
		" in "object))))

