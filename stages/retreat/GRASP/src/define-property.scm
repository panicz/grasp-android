
(import (srfi :17))
(import (define-syntax-rule))
(import (hash-table))

(define-syntax-rule (property (object) default)
  (let* ((override (make-weak-key-hash-table))
         (getter (lambda (object)
                   (hash-ref override object
                             (lambda () default)))))
    (set! (setter getter) (lambda (arg value)
                            (hash-set! override arg value)))
    (set-procedure-property! getter 'override override)
    getter))

;; property+ is like property but it stores the default
;; value for every enquired object
(define-syntax-rule (property+ (object) default)
  (let* ((override (make-weak-key-hash-table))
         (getter (lambda (object)
                   (hash-ref+ override object
                              (lambda () default)))))
    (set! (setter getter) (lambda (arg value)
                            (hash-set! override arg value)))
    (set-procedure-property! getter 'override override)
    getter))

(define-syntax-rule (define-property (property-name object) default)
  (define property-name
    (property (object) default)))

(define-syntax-rule (define-property+ (property-name object) default)
  (define property-name
    (property+ (object) default)))

(define-syntax-rule (unset! (property object))
  (let ((override (procedure-property property 'override)))
    (hash-remove! override object)))

(define-syntax-rule (update! (property object) expression)
  (let ((value expression))
    (unless (equal? (property object) value)
      (set! (property object) value))))
