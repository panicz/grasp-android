(import (define-syntax-rule))
(import (assert))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (define-property))
(import (define-parameter))
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
(import (document-operations))
(import (editor-operations))

(define-interface DocumentOperation ()
  (apply! document::pair)::void
  (inverse)::DocumentOperation
  ;;(merge previous-operation ::DocumentOperation)::DocumentOperation
  )

(define-type (Move source: Cursor
		   target: Cursor)
  implementing DocumentOperation
  with
  ((apply! document::pair)::void
   (let ((item (take-cell! at: source
			   from: document)))
     (splice! item into: document at: target)))

  ((inverse)
   (Move source: target
	 target: source)))

#|
(define-type (Remove element: Element
		     from: Cursor)
  implementing DocumentOperation
  with
  ((apply! document::pair)::void
   ...)
  ((inverse)::void
   (Insert element: element
	   at: from)))

(define-type (Insert element: Element
		     at: Cursor)
  implementing DocumentOperation
  with
  ((apply! document::pair)::void
   ...)
  ((inverse)::void
   (Remove element: element
	   from: at)))
|#


(define-object (History document::pair)
  (define fronts ::(list-of (list-of DocumentOperation)) '())

  (define undo-step ::int 0)
  
  (define (undo!)::void
    (and-let* ((`(,timeline . ,_) fronts)
	       (`(,last-action . ,_) (drop undo-step timeline))
	       (operation ::DocumentOperation last-action)
	       (inverse ::DocumentOperation (operation:inverse)))
      (inverse:apply! document)
      (set! undo-step (+ undo-step 1))))

  (define (redo!)::void
    (and-let* (((is undo-step > 0))
	       (`(,timeline . ,_) fronts)
	       (`(,undone-action . ,_) (drop (- undo-step 1)
					     timeline))
	       (operation ::DocumentOperation undone-action))
      (operation:apply! document)
      (set! undo-step (- undo-step 1))))

  (define (record! operation ::DocumentOperation)::void
    (cond ((null? fronts)
	   (set! fronts (cons (cons operation '()) fronts)))
	  ((is undo-step > 0)
	   (set! fronts (cons (cons operation
				    (car fronts))
			      fronts))
	   (set! undo-step 0))
	  (else
	   (set-car! fronts (cons operation
				  (car fronts)))))
    ))

(define-property+ (history document::pair)::History
  (History document))
