(import (define-property))

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
