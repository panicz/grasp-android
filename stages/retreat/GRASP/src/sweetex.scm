(import (define-syntax-rule))

(define-syntax-rule (use-sweetex)
  (begin 
    (import (define-syntax-rule))
    (import (assert))
    (import (conversions))
    (import (define-interface))
    (import (define-type))
    (import (examples))
    (import (infix))
    (import (match))))
