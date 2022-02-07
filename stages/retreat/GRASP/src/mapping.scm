(import (srfi :17))
(import (define-syntax-rule))
(import (hash-table))

(define-syntax-rule (mapping (object) default)
  (let* ((entries (make-hash-table))
         (getter (lambda (object)
                   (hash-ref entries object
                             (lambda () default)))))
    (set! (setter getter) (lambda (arg value)
                            (hash-set! entries arg value)))
    (set-procedure-property! getter 'table entries)
    getter))

(define (keys dict)
  (let ((table (procedure-property dict 'table)))
    (invoke (as java.util.Map table) 'keySet)))

(define-syntax-rule (define-mapping (mapping-name key) default-value)
  (define mapping-name
      (mapping (key) default-value)))
