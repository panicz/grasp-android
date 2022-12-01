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

;; take-cell-at! returns either a cons-cell whose
;; car is the desired object, or a head/tail-separator
;; (consider: what to do when cursor points to a space?
;; we can safely return #!null, because it's different
;; than returning (#!null))
(define (take-cell-at! #!optional
		       (cursor::Cursor (the-cursor))
		       (expression::pair (the-document)))
  (match cursor
    (`(,,@(isnt _ integer?) . ,root)
     (take-cell-at! root expression))
    
    (`(,,@(is _ <= 1) ,parent-index . ,root)
     (let* ((grandparent ::pair
			 (cursor-ref expression
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
     (let* ((parent ::pair (cursor-ref expression
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
     expression)))

(define (take-part-at! #!optional
		       (cursor::Cursor (the-cursor))
		       (object (the-document)))
  (cond #;((Indexable? object)
	 (invoke (as Indexable object) 
'take-part-at! cursor))

   ((pair? object)
    (take-cell-at! cursor object))

   (else
    (error "Don't know how to take "cursor
	   " from "object))))

(e.g.
 (let* ((document `(,1 ,3 ,5))
	(taken (take-cell-at! '(3) document)))
   (and (equal? document '(1 5))
	(equal? taken '(3)))))

(e.g.
 (let* ((document `(,1 ,3 ,5))
	(taken (take-cell-at! '(5) document)))
   (and (equal? document '(1 3))
	(equal? taken '(5)))))

(e.g.
 (let* ((document `((,1 ,3 ,5)))
	(taken (take-cell-at! '(1 1) document)))
   (and (equal? document '((3 5)))
	(equal? taken '(1)))))

(e.g.
 (let* ((document `((,1 . ,5)))
	(taken (take-cell-at! '(3 1) document)))
   (and (equal? document '((1 5)))
	(head/tail-separator? taken))))

(e.g.
 (let* ((document `((,1 . ,5)))
	(taken (take-cell-at! '(1 1) document)))
   (and (equal? document '((5)))
	(equal? taken '(1)))))

(e.g.
 (let* ((document `((,1 . ,5)))
	(taken (take-cell-at! '(5 1) document)))
   (and (equal? document '((1)))
	(equal? taken '(5)))))

(define (put-into-cell-at! cursor::Cursor
			   element
			   #;in
			   #!optional
			   (document (the-document)))
  ::boolean
  (assert (or (and (pair? element)
		   (list? (cdr element)))
	      (head/tail-separator? element)))
  (match cursor
    (`(,,@(isnt _ integer?) . ,root)
     (put-into-cell-at! root element document))

    (`(,,@(is _ <= 1) ,parent-index . ,root)
     (assert (pair? element))
     (let* ((grandparent ::pair (cursor-ref
				 document root))
	    (parent (drop (quotient parent-index 2)
			  grandparent)))
       (set! (last-tail element) (car parent))
       (set! (car parent) element)
       #t))

    (`(,index . ,root)
     (let* ((parent (cursor-ref document root))
	    (irrelevant (- (quotient index 2) 1))
	    (preceding (drop irrelevant parent)))
       (cond ((pair? element)
	      (set! (last-tail element)
		(cdr preceding))
	      (set! (cdr preceding) element)
	      #t)
	     
	     ((null? (cdr (cdr preceding)))
	      (assert (head/tail-separator?
		       element))
	      (set! (cdr preceding)
		(car (cdr preceding)))
	      (update! (dotted? preceding) #t)
	      #t)

	     (else
	      #f))))
    (_
     #f)
  ))

(e.g.
 (let ((document `((,1 ,5))))
   (put-into-cell-at! '(2 1) `(,3) document)
   document) ===> ((1 3 5)))

(e.g.
 (let ((document `((,1 ,7))))
   (put-into-cell-at! '(2 1) `(,3 ,5) document)
   document) ===> ((1 3 5 7)))

(e.g.
 (let ((document `((,1 ,5))))
   (put-into-cell-at! '(2 1)
		      head/tail-separator
		      document)
   document) ===> ((1 . 5)))

(e.g.
 (let ((document `((,3 ,5))))
   (put-into-cell-at! '(0 1) `(,1) document)
   document) ===> ((1 3 5)))

(e.g.
 (let ((document `((,5 ,7))))
   (put-into-cell-at! '(0 1) `(,1 ,3) document)
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
