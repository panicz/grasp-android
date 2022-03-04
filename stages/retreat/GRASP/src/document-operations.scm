(import (srfi :11))
(import (define-interface))
(import (define-type))
(import (indexable))
(import (match))


;; we should patch Kawa to make a trivial SExpr
;; interface shared by lists and atoms
(define-alias SExpr java.lang.Object)

(define-type (Area left: real
		   top: real
		   width: real
		   height: real))

(define-interface Operation ()
  (reciprocal)::Operation
  ;;(perform document::Document)::void
  )

(define (reciprocal operation::Operation)::Operation
  (invoke operation 'reciprocal))

(define-type (MoveExpression from: Cursor to: Cursor)
  implementing Operation
  with
  ((reciprocal)::Operation
   (MoveExpression from: to to: from)))

(define-type (Remove expression: SExpr at: Cursor)
  implementing Operation
  with
  ((reciprocal)::Operation
   (Insert expression: expression at: at)))

(define-type (Insert expression: SExpr at: Cursor)
  implementing Operation
  with
  ((reciprocal)::Operation
   (Remove expression: expression at: at)))

(define-type (CreateBox spanning: Area at: Cursor)
  implementing Operation
  with
  ((reciprocal)::Operation
   (SpliceBox spanning: spanning at: at)))

(define-type (SpliceBox spanning: Area at: Cursor)
  implementing Operation
  with
  ((reciprocal)::Operation
   (CreateBox spanning: Area at: Cursor)))

(define-type (ImproperizeBox at: Cursor)
  implementing Operation
  with
  ((reciprocal)::Operation
   (ProperizeBox at: Cursor)))

(define-type (ProperizeBox at: Cursor)
  implementing Operation
  with
  ((reciprocal)::Operation
   (ImproperizeBox at: Cursor)))

(define-interface Document ()
  (take-expression-at! cursor::Cursor)::SExpr
  (put-at! cusor::Cursor expression::SExpr)::void
  )

(define (perform! operation document)::void
  (match operation
    ((MoveExpression from: source to: destination)
     (let ((expression (document:take-expression-at! source)))
       (document:put-at! destination expression)))

    ((Remove expression: expr at: cursor)
     (let ((target (document:take-expression-at! cursor)))
       (assert (equal? expr target))))
    
    ((Insert expression: expr at: cursor)
     (document:put-at! cursor expr))

    ((CreateBox spanning: area at: cursor)
     ;; tutaj bedziemy musieli operowac nie na s-wyrazeniach,
     ;; tylko na "rozciaglej reprezentacji S-wyrazen"
     ...)

    ((SpliceBox spanning: area at: cursor)
     ;; tutaj mamy tylko slaba asercje, ze obszar
     ;; danego wyrazenia jest taki sam, jak obszar
     ...)

    ((ProperizeBox at: cursor)
     ....)

    ((ImproperizeBox at: cursor)
     ....)))

(e.g.
 (let* ((input (list 1 3 5))
	(taken (take-part-at! 1 input)))
   (and (eqv? taken 1)
	(equal? input '(3 5)))))

(e.g.
 (let* ((input (list 1 3 5))
	(taken (take-part-at! 3 input)))
   (and (eqv? taken 3)
	(equal? input '(1 5)))))

(e.g.
 (let* ((input (list 1 3 5))
	(taken (take-part-at! 5 input)))
   (and (eqv? taken 5)
	(equal? input '(1 3)))))

(e.g.
 (let* ((input (list 1 3))
	(taken (take-part-at! 1 input)))
   (and (eqv? taken 1)
	(equal? input '(3)))))

(e.g.
 (let* ((input (list 1 3))
	(taken (take-part-at! 3 input)))
   (and (eqv? taken 3)
	(equal? input '(1)))))

;; note that this cannot be implemented, because
;; we cannot convert a pair into an empty list:
;;
;; (e.g.
;;   (let* ((input (list 1))
;;          (taken (take-part-at! 1 input)))
;;     (and (eqv? taken 1)
;;          (equal? input '()))))
;;
;; but we can perform this operation from
;; the perspective of the parent (if there is one),

(e.g.
 (let* ((input `((,1 ,3 ,5)))
	(result (take-expression-at! '(1 1) input)))
   (and (equal? result '(1))
	(equal? input '((3 5))))))

(e.g.
 (let* ((input `((,1 ,3)))
	(result (take-expression-at! '(1 1) input)))
   (and (equal? result '(1))
	(equal? input '((3))))))

(e.g.
 (let* ((input `((,1)))
	(result (take-expression-at! '(1 1) input)))
   (and (equal? result '(1))
	(equal? input '(())))))

(e.g.
 (let* ((input `((,1 ,3 ,5)))
	(result (take-expression-at! '(3 1) input)))
   (and (equal? result '(3))
	(equal? input '((1 5))))))

(1 3) => (1 . (3))

(1 3) => (1 . 3)

(1) => (1 . ()


(e.g.
 (let* ((input `((,1 ,3 ,5)))
	(result (take-expression-at! '(3 1) input)))
   (and (equal? result '(3))
	(equal? input '((1 5))))))



(define (take-expression-at! cursor expression)
  (match (base-cursor cursor expression)
    (`(,index . ,context)
     (let ((parent (cursor-ref expression context)))
       (take-part-at! index parent)))
    (_
     expression)))
