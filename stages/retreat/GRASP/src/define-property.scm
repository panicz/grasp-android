(import (srfi :17))
(import (define-syntax-rule))
(import (hash-table))

(define-syntax-rule (define-property (property-name object) default)
  (define property-name
    (let* ((override (make-weak-key-hash-table))
	   (getter (lambda (object)
		     (hash-ref override object
			       (lambda () default)))))
      (set! (setter getter) (lambda (arg value)
			      (hash-set! override arg value)))
      getter)))
