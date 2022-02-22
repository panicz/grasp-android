(import (define-syntax-rule))

(define-syntax object-definition
  (syntax-rules (define :: set!)

    ((object-definition (object-name . args) (supers ...)
			slots methods
			(:: type . spec))
     (object-definition (object-name . args) (supers ... type)
			slots methods
			spec))
    
    ((object-definition (object-name . args) supers
			slots (methods ...)
			((define (method . params) . body) . spec))
     (object-definition (object-name . args) supers
			slots (methods ... ((method . params) . body))
			spec))

    ((object-definition (object-name . args) supers
			(slots ...) methods
			((define slot :: type value) . spec))
     (object-definition (object-name . args) supers
			(slots ... (slot :: type init: value)) methods
			spec))

    ((object-definition (object-name . args) supers
			(slots ...) methods
			((define slot :: type) . spec))
     (object-definition (object-name . args) supers
			(slots ... (slot :: type)) methods
			spec))
    
    ((object-definition (object-name . args) supers
			(slots ...) methods
			((define slot value) . spec))
     (object-definition (object-name . args)
			supers (slots ... (slot init: value)) methods
			spec))

    ((object-definition (object-name) (supers ...)
			(slots ...) (methods ...) ())
     (define-simple-class object-name (supers ...)
       slots ... methods ...))

    ((object-definition (object-name . args) (supers ...)
			slots (methods ...)
			((set! property value) . init))
     (object-definition (object-name) (supers ...) slots
			(methods
			 ...
			 ((*init* . args)
			  (set! property value) . init))
			()))
    
    ((object-definition (object-name . args) (supers ...)
			slots (methods ...)
			((super . args*) . init))
     (object-definition (object-name) (supers ... super) slots
			(methods
			 ...
			 ((*init* . args)
			  (invoke-special super (this) '*init* . args*)
			  . init))
			()))

    ))

(define-syntax-rule (define-object (object-name . args) . spec)
  (object-definition (object-name . args)
		     #;supers () #;slots () #;methods () #;spec spec))
