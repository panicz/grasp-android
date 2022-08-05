(import (functions))
(import (fundamental))
(import (indexable))
(import (space))
(import (cursor))
(import (primitive))
(import (symbol))
(import (document-operations))
(import (infix))
(import (match))
(import (print))

(define (delete! position::Index)::void
  (let* ((target (the-expression)))
    (cond
     ((is target instance? Symbol)
      (cond ((is 0 <= position < (symbol-length target))
	     (delete-char! target position)
	     (when (= (symbol-length target) 0)
	       (take-cell-at! (cursor-tail))
	       (set! (the-cursor)
		     (cursor-climb-back
		      (recons (- (head (cursor-tail))
				 1)
			      (tail (cursor-tail)))))))))
     ((is target instance? Space)
      (if (or (is position > (first-index target))
	      (and (is position = (first-index target))
		   (or (and-let* ((`(#\] . ,_) (cursor-advance))))
		       (and-let* ((`(#\[ . ,_) (cursor-retreat)))))))
	  (delete-space! target position))))))

(define (delete-forward!)::void
  (let ((target (the-expression)))
    (cond ((and (pair? target)
		(pair? (the-cursor))
		(eqv? (cursor-head) (first-index target)))
	   (let ((new-cursor (cursor-retreat)))
	     (take-cell-at!)
	     (set! (the-cursor) new-cursor)))
	  (else
	   (delete! (cursor-head))))))

(define (delete-backward!)::void
  (let ((target (the-expression)))
    (cond ((and (pair? target)
		(eqv? (cursor-head) (last-index target)))
	   (let ((new-cursor (cursor-climb-back
			      (cursor-back (cursor-tail)))))
	     (take-cell-at!)
	     (set! (the-cursor) new-cursor)))
	  (else
	   (set! (the-cursor)
		 (cursor-climb-back (cursor-back)))
	   (delete! (cursor-head))))))

(define (insert-character! c::char)::void
  (and-let* ((`(,tip . ,stem) (the-cursor))
	     (`(,top . ,root) stem)
	     (parent (the-expression at: root))
	     (target (part-at top parent)))
    (cond
     ((is c memq '(#\[ #\( #\{))
      (cond
       ((is target instance? Space)
	(put-into-cell-at! root #;(cursor-tail) (cons '() '()))
	(set! (the-cursor)
	      (recons* 0 0 (+ (head (cursor-tail)) 1)
		       (tail (cursor-tail)))))
       ((eqv? (cursor-head) #\])
	(set! (the-cursor)
	      (recons #\[ (cursor-tail))))

       (else
	(let ((target (take-cell-at! (cursor-tail))))
	  (put-into-cell-at! (cursor-tail) (cons target '()))
	  (set! (the-cursor)
		(recons #\[ (cursor-tail)))))))

     ((is c memq '(#\] #\) #\}))
      (set! (the-cursor)
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
		      (owner (drop (quotient top 2) parent))
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
	  (set! (the-cursor)
	    (recons (+ (cursor-head) 1)
		    (cursor-tail))))))
     ((is target instance? Space)

      (cond
       ((is c memq '(#\space #\newline))
	(insert-whitespace! c target (cursor-head))
	(set! (the-cursor)
	      (recons (+ (cursor-head) 1)
		      (cursor-tail)))
	)

       ((is c memq '(#\. #\|))
	(put-into-cell-at! (cursor-tail)
			   head/tail-separator
			   (the-document))
	(times 2 cursor-advance!))
       
       (else
	(let* ((space-after (split-space!
			     target
			     (cursor-head))))
	  (put-into-cell-at!
	   (cursor-tail)
	   (cons (Symbol (list->string (list c)))
		 '()))
	  (set! (the-cursor)
	    (recons* 1 (+ (head (cursor-tail)) 1)
		     (tail (cursor-tail))))))))
     )))


