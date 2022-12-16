(import (srfi :11))
(import (define-interface))
(import (define-type))
(import (hash-table))
(import (define-property))
(import (fundamental))
(import (indexable))
(import (space))
(import (cursor))
(import (assert))
(import (match))
(import (examples))
(import (infix))
(import (functions))
(import (keyword-arguments))
(import (print))

;; take-cell! returns either a cons-cell whose
;; car is the desired object, or a head/tail-separator
;; (consider: what to do when cursor points to a space?
;; we can safely return #!null, because it's different
;; than returning (#!null))
(define/kw (take-cell! at: cursor::Cursor := (the-cursor)
		       from: document::pair := (the-document))
  (match cursor
    (`(,,@(isnt _ integer?) . ,root)
     (take-cell! at: root from: document))
    
    (`(,,@(is _ <= 1) ,parent-index . ,root)
     (let* ((grandparent ::pair
			 (cursor-ref document
				     root))
	    (cell (drop (quotient parent-index 2)
			grandparent))
	    (removed (car cell)))
       (if (dotted? removed)
	   (let ((new (cons (cdr removed) '())))
	     (tail-space-to-head removed new)
	     (set! (car cell) new)
	     (unset! (dotted? removed)))
	   (set! (car cell) (cdr removed)))
       (set! (cdr removed) '())
       removed))
    
    (`(,index . ,root)
     (let* ((parent ::pair (cursor-ref document
				       root))
	    (index (quotient index 2))
	    (irrelevant (- index 1)))
       (define (remove-tail! preceding)
	 (let ((removed (cdr preceding)))
	   (join-spaces! (post-head-space preceding)
			 (post-head-space removed))
	   (set! (cdr preceding) (cdr removed))
	   (set! (cdr removed) '())
	   removed))
	 
       (if (is irrelevant > 0)
	   (let* ((irrelevant (- irrelevant 1))
		  (preceding (drop irrelevant
				   parent)))
	     (if (dotted? preceding)
		 (let* ((removed (tail-space-to-head
				  preceding
				  (cons (cdr
					 preceding)
					'()))))
		   (set! (cdr preceding) '())
		   (unset! (dotted? preceding))
		   removed)
		 (remove-tail! (cdr preceding))))
	   (let ((preceding (drop irrelevant
				  parent)))
	     (if (dotted? preceding)
		 (let* ((added (cons (cdr
				      preceding)
				     '())))
		   (tail-space-to-head preceding
				       added)
		   (set! (cdr preceding) added)
		   (unset! (dotted? preceding))
		   head/tail-separator)
		 (remove-tail! preceding))))))
    (_
     document)))

(e.g.
 (let* ((document `(,1 ,3 ,5))
	(taken (take-cell! at: '(3) from: document)))
   (and (equal? document '(1 5))
	(equal? taken '(3)))))

(e.g.
 (let* ((document `(,1 ,3 ,5))
	(taken (take-cell! at: '(5) from: document)))
   (and (equal? document '(1 3))
	(equal? taken '(5)))))

(e.g.
 (let* ((document `((,1 ,3 ,5)))
	(taken (take-cell! at: '(1 1) from: document)))
   (and (equal? document '((3 5)))
	(equal? taken '(1)))))

(e.g.
 (let* ((document `((,1 . ,5)))
	(taken (take-cell! at: '(3 1) from: document)))
   (and (equal? document '((1 5)))
	(head/tail-separator? taken))))

(e.g.
 (let* ((document `((,1 . ,5)))
	(taken (take-cell! at: '(1 1) from: document)))
   (and (equal? document '((5)))
	(equal? taken '(1)))))

(e.g.
 (let* ((document `((,1 . ,5)))
	(taken (take-cell! at: '(5 1) from: document)))
   (and (equal? document '((1)))
	(equal? taken '(5)))))



(define/kw (splice! element
		    into: document::pair := (the-document)
		    at: cursor::Cursor := (the-cursor))
  ::boolean
  (match cursor
    (`(,tip ,top . ,root)
     (let* ((grandpa (cursor-ref document root))
	    (parent (part-at top grandpa))
	    (target (part-at tip parent)))
       (cond
	((and (Space? target)
	      (pair? grandpa)
	      (eq? parent target))
	 (if (is top <= 1)
	     (and-let* ((`(,heir . ,origin) root)
			(predecesor ::pair (cursor-ref document origin))
			(parent (drop (quotient heir 2) predecesor)))
	       (set! (last-tail element) (car parent))
	       (set! (car parent) element) #t)

	     (let* ((irrelevant (- (quotient top 2) 1))
		    (before (drop irrelevant grandpa)))
	       (cond ((pair? element)
		      (set! (last-tail element) (cdr before))
		      (set! (cdr before) element) #t)
		     
		     ((null? (cdr (cdr before)))
		      (assert (head/tail-separator? element))
		      (set! (cdr before) (car (cdr before)))
		      (update! (dotted? before) #t) #t)

		     (else
		      (WARN "Attempt to splice "element
			    " in non-tail position") #f))

	       )))
	(else
	 (WARN "unhandled case: "
	       `(splice! ,element into: ,document at: ,cursor)) #f)
	)))))

(e.g.
 (let ((document `((,1 ,5))))
   (splice! `(,3) into: document at: '(0 2 1))
   document) ===> ((1 3 5)))

(e.g.
 (let ((document `((,1 ,7))))
   (splice! `(,3 ,5) into: document at: '(0 2 1))
   document) ===> ((1 3 5 7)))

(e.g.
 (let ((document `((,1 ,5))))
   (splice! head/tail-separator
	    into: document at: '(0 2 1))
   document) ===> ((1 . 5)))

(e.g.
 (let ((document `((,3 ,5))))
   (splice! `(,1) into: document at: '(0 0 1))
   document) ===> ((1 3 5)))

(e.g.
 (let ((document `((,5 ,7))))
   (splice! `(,1 ,3) into: document at: '(0 0 1))
   document) ===> ((1 3 5 7)))

(define (replace-expression! #!key
			     (at::Cursor (the-cursor))
			     (with)
			     (in (the-document)))
  (match at
    (`(,,@(isnt _ integer?) . ,cursor)
     (replace-expression! at: cursor with: with in: in))

    (`(,,@(isnt _ odd?) . ,cursor)
     (replace-expression! at: cursor with: with in: in))
    
    (`(,index . ,cursor)
     (let* ((parent (cursor-ref in cursor))
	    (previous-index (- (quotient index 2) 1)))
       (cond ((head/tail-separator? with)
	      (unless (or (head/tail-separator? (cell-index parent
							    index))
			  (is previous-index < 0))
		(let ((cell (drop previous-index parent)))
		  (assert (null? (cdddr cell)))
		  (set! (cdr cell) (caddr cell))
		  (update! (dotted? cell) #t)
		  in)))
	     ((head/tail-separator? (cell-index parent index))
	      (unless (is previous-index < 0)
		(let ((cell (drop previous-index parent)))
		  (set! (cdr cell) (cons with (cons (cdr cell) '())))
		  (update! (dotted? cell) #f)
		  in)))
	     (else 
	      (set! (cell-index parent index) with)
	      in))))))

(e.g.
 (let ((document `((,1 ,2 . ,3))))
   (replace-expression! at: '(1 1)
			with: 'x
			in: document)
   document) ===> ((x 2 . 3)))

(e.g.
 (let ((document `((,1 ,2 . ,3))))
   (replace-expression! at: '(3 1)
			with: 'x
			in: document)
   document) ===> ((1 x . 3)))

(e.g.
 (let ((document `((,1 ,2 . ,3))))
   (replace-expression! at: '(5 1)
			with: 'x
			in: document)
   document) ===> ((1 2 x 3)))

(e.g.
 (let ((document `((,1 ,2 . ,3))))
   (replace-expression! at: '(7 1)
			with: 'x
			in: document)
   document) ===> ((1 2 . x)))

(e.g.
 (let ((document `((,1 ,2 ,3))))
   (replace-expression! at: '(3 1)
			with: head/tail-separator
			in: document)
   document) ===> ((1 . 3)))
