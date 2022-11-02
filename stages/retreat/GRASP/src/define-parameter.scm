(import (default-value))

(define-syntax define-parameter
  (syntax-rules (::)
    
    ((_ (parameter-name) :: type initial-value)
     (define-early-constant parameter-name :: parameter[type]
       (make-parameter initial-value)))

    ((_ (parameter-name) :: type)
     (define-early-constant parameter-name :: parameter[type]
       (make-parameter (default-value type))))
    
    ((_ (parameter-name) initial-value)
     (define-early-constant parameter-name :: parameter
       (make-parameter initial-value)))

    ((_ (parameter-name))
     (define-early-constant parameter-name :: parameter
       (make-parameter #!null)))
    ))

(define-syntax define-parameter+
  (syntax-rules (::)
    
    ((_ (parameter-name) :: type initial-value)
     (define-constant parameter-name :: parameter[type]
       (make-parameter initial-value)))

    ((_ (parameter-name) :: type)
     (define-constant parameter-name :: parameter[type]
       (make-parameter (default-value type))))
    
    ((_ (parameter-name) initial-value)
     (define-constant parameter-name :: parameter
       (make-parameter initial-value)))

    ((_ (parameter-name))
     (define-constant parameter-name :: parameter
       (make-parameter #!null)))
    ))
    

