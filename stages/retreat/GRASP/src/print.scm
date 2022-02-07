(import (define-syntax-rule))

(define-syntax-rule (print elements ...)
  (display elements)
  ...
  (newline))
