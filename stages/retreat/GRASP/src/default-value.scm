(import (define-property))

(define-property (default-value type) #!null)

(set! (default-value real) 0.0)
(set! (default-value int) 0)
(set! (default-value string) "")
(set! (default-value list) '())
(set! (default-value boolean) #f)
