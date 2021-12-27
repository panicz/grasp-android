(import (define-syntax-rule))

(define-syntax-rule (define-interface name supers prototypes ...)
  (interface-definition name supers (prototypes ...) ()))

(define-syntax interface-definition
  (syntax-rules ()
    ((_ name supers () methods)
     (define-simple-class name supers interface: #t . methods))
    
    ((_ name supers (method :: result . rest) (methods ...))
     (interface-definition
      name supers rest
      (methods ... (method :: result #!abstract))))
    ))


(define-syntax-rule (define-external-interface name supers protos ...)
  (external-interface-definition name supers (protos ...) () ()))

(define-syntax invoke-method
  (syntax-rules ()
    ((_ target method-name () args)
     (invoke target method-name . args))

    ((_ target method-name (arg :: type . args) (processed ...))
     (invoke-method target method-name args (processed ... arg)))

    ((_ target method-name (arg . args) (processed ...))
     (invoke-method target method-name args (processed ... arg)))))

(define-syntax external-interface-definition
  (syntax-rules ()
    ((_ name supers () methods external-defs)
     (begin
       (define-simple-class name supers interface: #t . methods)
       . external-defs))
    
    ((_ name supers ((method . args) :: result . rest)
	(methods ...) (defs ...))
     (external-interface-definition
      name supers rest
      (methods ... ((method . args) :: result #!abstract))
      (defs ... (define (method self :: name . args) :: result
		  (invoke-method  self 'method args ())))))
    ))
