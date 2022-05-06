(import (functions))
(import (indexable))
(import (space))
(import (cursor))
(import (tile))
(import (primitive))
(import (symbol))
(import (document-operations))
(import (infix))
(import (match))
(import (print))

(define (delete! position::Index)::void
  (let* ((target (cursor-ref)))
    (cond
     ((is target instance? Symbol)
      (cond ((is 0 <= position < (symbol-length target))
	     (delete-char! target position)
	     (when (= (symbol-length target) 0)
	       (take-cell-at! (cursor-tail))
	       (set! (current-cursor)
		     (cursor-climb-back
		      (recons (- (head (cursor-tail))
				 1)
			      (tail (cursor-tail)))))))))
     ((is target instance? Space)
      (if (is position > (first-index target))
	  (delete-space! target position))))))

(define (delete-forward!)::void
  (let ((target (cursor-ref)))
    (cond ((and (pair? target)
		(pair? (current-cursor))
		(eqv? (cursor-head) (first-index target)))
	   (let ((new-cursor (cursor-retreat)))
	     (take-cell-at!)
	     (set! (current-cursor) new-cursor)))
	  (else
	   (delete! (cursor-head))))))

(define (delete-backward!)::void
  (let ((target (cursor-ref)))
    (cond ((and (pair? target)
		(eqv? (cursor-head) (last-index target)))
	   (let ((new-cursor (cursor-climb-back
			      (cursor-back (cursor-tail)))))
	     (take-cell-at!)
	     (set! (current-cursor) new-cursor)))
	  (else
	   (set! (current-cursor)
		 (cursor-climb-back (cursor-back)))
	   (delete! (cursor-head))))))

(define (insert-character! c::char)::void
  (and-let* ((`(,tip . ,stem) (current-cursor))
	     (`(,top . ,root) stem)
	     (parent (expression-at root))
	     (owner (drop (quotient top 2) parent))
	     (target (part-at top parent)))
    (cond
     ((is c memq '(#\[ #\( #\{))
      (put-into-cell-at! (cursor-tail) (cons '() '()))
      (set! (current-cursor)
	(recons* 0 0 (+ (head (cursor-tail)) 1)
		 (tail (cursor-tail)))))

     ((is c memq '(#\] #\) #\}))
      (set! (current-cursor)
	    (recons #\] root)))
     
     ((is target instance? Symbol)
      (cond
       ((or (eq? c #\space) (eq? c #\newline))
	(cond ((eqv? (cursor-head) (first-index target))
	       (let ((preceding-space (part-at
				       (previous-index
					top parent)
				       parent)))
		 (insert-whitespace! c preceding-space
				     (last-index
				      preceding-space))))
	      ((eqv? (cursor-head) (last-index target))
	       (let ((following-space (part-at
				       (next-index
					top parent)
				       parent)))
		 (insert-whitespace! c following-space
				     (first-index
				      following-space))
		 (cursor-advance!)))
	      (else
	       (let* ((suffix (symbol-subpart target tip))
		      (cell (cons suffix (tail owner))))
		 (truncate-symbol! target tip)
		 (set! (tail owner) cell)
		 (set! (post-head-space cell)
		   (post-head-space owner))
		 (set! (post-head-space owner)
		   (Space fragments: (if (eq? c #\newline)
					 (cons 0
					       (cons 0 '()))
					 (cons 1 '()))))
		 (cursor-advance!)))))
       
	 (else
	  (insert-char! c target (cursor-head))
	  (set! (current-cursor)
	    (recons (+ (cursor-head) 1)
		    (cursor-tail))))))
     ((is target instance? Space)

      (cond
       ((eq? c #\space)
	(insert-space! target (cursor-head))
	(set! (current-cursor)
	      (recons (+ (cursor-head) 1)
		      (cursor-tail)))
	)

       ((eq? c #\newline)
	(insert-break! target (cursor-head)))
              
       (else
	(let* ((space-after (split-space!
			     target
			     (cursor-head))))
	  (put-into-cell-at!
	   (cursor-tail)
	   (cons (Symbol (list->string (list c)))
		 '()))
	  (set! (current-cursor)
	    (recons* 1 (+ (head (cursor-tail)) 1)
		     (tail (cursor-tail))))))))
     )))


