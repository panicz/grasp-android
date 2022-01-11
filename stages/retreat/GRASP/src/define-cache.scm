(import (srfi :17))
(import (define-syntax-rule))
(import (hash-table))
(import (primitive))

(define-syntax-rule (cache (object) formula)
  (let* ((override (make-weak-key-hash-table))
         (getter (lambda (object)
                   (hash-ref override object
                             (lambda ()
                               (let ((result formula))
                                 (hash-set! override object result)
                                 result))))))
    (set! (setter getter) (lambda (arg value)
                            (hash-set! override arg value)))
    getter))

(define-syntax-rule (define-cache (property-name object) default)
  (define property-name
    (cache (object) default)))

(define-cache (heads tail)
  (cache (head)
    (cons head tail)))

(define (recons head tail)::cons
  ((heads tail) head))