(import (define-syntax-rule))

(define-syntax-rule (define-synonym synonym existing-name)
  (define-syntax-rule (synonym . args)
    (existing-name . args)))
