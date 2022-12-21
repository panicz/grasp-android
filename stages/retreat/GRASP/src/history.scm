(import (define-syntax-rule))
(import (assert))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (define-property))
(import (define-parameter))
(import (keyword-arguments))
(import (default-value))
(import (mapping))
(import (fundamental))
(import (infix))
(import (match))
(import (functions))
(import (for))
(import (while))
(import (indexable))
(import (primitive))
(import (cursor))
(import (document-operations))
(import (editor-operations))

(define-interface Edit ()
  (apply! document::pair)::void
  (inverse)::Edit
  )

(define-type (Move from: Cursor
		   to: Cursor
		   with-shift: int)
  implementing Edit
  with
  ((apply! document::pair)::void
   (let ((item (take-cell! at: from from: document)))
     (splice! item into: document at: to)
     #!null))

  ((inverse)::Edit
   (match (this)
     ((Move from: `(,s0 . ,source)
            to: `(,d0 ,d1 . ,destination)
	    with-shift: s)
      (Move from: (recons (+ d1 1) destination)
            to: (recons* s (- s0 1) source)
	    with-shift: d0)))))

(define-type (Remove element: (either pair HeadTailSeparator)
		     from: Cursor
		     with-shift: int)
  implementing Edit
  with
  ((apply! document::pair)::void
   (let ((item (take-cell! at: from from: document)))
     (assert (eq? item element))
     (values)))
  ((inverse)::Edit
   (match from
     (`(,tip . ,root)
      (Insert element: element
	      at: (recons* with-shift (- tip 1) root))))))

(define-type (Insert element: (either pair HeadTailSeparator)
		     at: Cursor)
  implementing Edit
  with
  ((apply! document::pair)::void
   (splice! element into: document at: at))
  ((inverse)::void
   (match at
     (`(,tip ,top . ,root)
      (Remove element: element
	      from: (recons (+ top 1) root)
	      with-shift: tip)))))

(define-object (History document::pair)
  (define fronts ::(list-of (list-of Edit)) '())

  (define undo-step ::int 0)
  
  (define (undo!)::void
    (and-let* ((`(,timeline . ,_) fronts)
	       (`(,last-action . ,_) (drop undo-step timeline))
	       (operation ::Edit last-action)
	       (inverse ::Edit (operation:inverse)))
      (inverse:apply! document)
      (set! undo-step (+ undo-step 1))))

  (define (redo!)::void
    (and-let* (((is undo-step > 0))
	       (`(,timeline . ,_) fronts)
	       (`(,undone-action . ,_) (drop (- undo-step 1)
					     timeline))
	       (operation ::Edit undone-action))
      (operation:apply! document)
      (set! undo-step (- undo-step 1))))

  (define (record! operation ::Edit)::void
    (cond ((null? fronts)
	   (set! fronts (cons (cons operation '()) fronts)))
	  ((is undo-step > 0)
	   (set! fronts (cons (cons operation
				    (car fronts))
			      fronts))
	   (set! undo-step 0))
	  ((and-let* ((`((,last-operation . ,_) . ,_) fronts)
		      ((Remove element: e 
			       from: source
			       with-shift: n) last-operation)
		      ((Insert element: e*
			       at: target) operation)
		      ((eq? e e*)))
	     (Move from: source
		   to: target
		   with-shift: n)) =>
		   (lambda (operation::Move)
		     (if (equal? operation:from operation:to)
			 (set! (car fronts) (cdr (car fronts)))
			 (set! (car (car fronts))
			   operation))))
	  #;((and-let* ((`((,last-operation . ,_) . ,_) fronts)
		      ((Insert element: e1
			       at: `(,a0 . ,a*)) last-operation)
		      ((Insert element: e2
			       at: `(,b0 . ,,a*)) operation)
		      ...)
	     (append! e1 e2)
	     (append! e2 e1)))
	  (else
	   (set-car! fronts (cons operation
				  (car fronts)))))
    ))

(define-property+ (history document::pair)::History
  (History document))

(define/kw (remove-element! at: cursor::Cursor
			    from: document := (the-document))
  ::Remove
  (let* ((shift (last-index (space-preceding cursor in: document)))
	 (element (take-cell! at: cursor from: document))
	 (history ::History (history document))
	 (action ::Remove (Remove element: element
				  from: cursor
				  with-shift: shift)))
    (history:record! action)
    action))

(define/kw (insert! element
		    into: document := (the-document)
		    at: cursor::Cursor)
  ::Insert
  (let ((action ::Insert (Insert element: element
				 at: cursor))
	(history ::History (history document)))
    (action:apply! document)
    (history:record! action)
    action))
