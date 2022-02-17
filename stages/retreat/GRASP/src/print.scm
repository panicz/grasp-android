(import (define-syntax-rule))

(define-syntax-rule (print elements ...)
  (display elements)
  ...
  (newline))

(define-syntax-rule (dump expr)
  (print 'expr ": "expr))
