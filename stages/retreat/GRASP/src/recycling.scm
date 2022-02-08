(import (define-syntax-rule))
(import (define-interface))
(import (define-property))
(import (define-type))
(import (infix))
(import (conversions))
(import (while))

(define-alias ArrayList java.util.ArrayList)

(define-interface Recycling ()
  ;; create a new instance of an object (of some uniform type),
  ;; possibly re-using the storage of some object from a previous round
  (recycled-element)::Object

  ;; mark that the current round of issuing objects
  ;; has ended. Further allocations might reuse the
  ;; objects from the previous round.
  (recycle)::void)

(define-property+ (recycling constructor)
  (object (Recycling)
    (storage :: ArrayList init-form: (ArrayList))
    (next-index :: int init-value: 0)
    ((recycled-element)::Object
     (cond ((is next-index < (storage:size))
	    (let ((result (storage next-index)))
	      (set! next-index (+ next-index 1))
	      result))
	   (else
	    (let ((result (constructor)))
	      (storage:add result)
	      (set! next-index (+ next-index 1))
	      result))))
    ((recycle)::void
     (while (is next-index < (storage:size))
       (storage:remove (- (storage:size) 1)))
     (set! next-index 0))))

(define-syntax-rule (Recycled constructor args ...)
  (let* ((reconstruct ::Recycling (recycling constructor))
	 (instance (reconstruct:recycled-element)))
    (set-fields! instance args ...)
    instance))

(define (recycle object-type)
  (invoke (as Recycling (recycling object-type)) 'recycle))
