(import (define-interface))
(import (define-property))
(import (define-type))
(import (define-object))
(import (mapping))
(import (indexable))
(import (tile))
(import (primitive))
(import (match))
(import (functions))
(import (cursor))
(import (document-operations))
(import (print))

(define-interface Interactive (Tile)
  ;; by convention, the return value of #true means
  ;; that the event was handled by the object,
  ;; and the value of #false means that it was ignored
  ;;
  ;; This is used by the event system to find out
  ;; whether the event has been consumed or not.
  (key-pressed key::char)::boolean
  (key-released key::char)::boolean
  
  (tapped x::real y::real)::boolean
  (pressed x::real y::real)::boolean
  (released x::real y::real)::boolean
  
  (dragged-over x::real y::real item::Tile*)::boolean
  (dragged-out x::real y::real item::Tile*)::boolean
  (dropped x::real y::real item::Tile*)::boolean
  
  (held x::real y::real)::boolean
  (double-tapped x::real y::real)::boolean
  (second-pressed x::real y::real)::boolean

  )

(define-object (Passive)::Interactive
  (define (typename)::String "Passive")

  (define (fields->string)::String "")

  (define (embedded-in? object)::boolean
    (instance? object Passive))

  (define (assign source::Struct)::Struct
    (this))

  (define (key-pressed key::char)::boolean #f)
  (define (key-released key::char)::boolean #f)
  
  (define (tapped x::real y::real)::boolean #f)
  (define (pressed x::real y::real)::boolean #f)
  (define (released x::real y::real)::boolean #f)
  
  (define (dragged-over x::real y::real item::Tile*)::boolean #f)
  (define (dragged-out x::real y::real item::Tile*)::boolean #f)
  (define (dropped x::real y::real item::Tile*)::boolean #f)
  
  (define (held x::real y::real)::boolean #f)
  (define (double-tapped x::real y::real)::boolean #f)
  (define (second-pressed x::real y::real)::boolean #f)
  
  (define (draw! context::Cursor)::void #!abstract)
  (define (extent)::Extent #!abstract)

  (Simple))
  
(define-interface Extension ()
  (create-from source::cons)::Interactive
  )

(define-mapping (extension keyword)
  (begin
    (WARN "no extension for "keyword)
    #f))

(define-property (origin enchanted)
  enchanted)

(define (enchant-expression! #!optional
			     #;at (cursor::Cursor (the-cursor))
				  #;in (document (the-document)))
  (parameterize ((evaluating? #t))
    (let ((target (the-expression)))
      (if (Interactive? target)
	  (let ((original (origin target)))
	    (unset! (origin target))
	    (replace-expression! at: cursor
				 with: original
				 in: document))
	  (and-let* ((target (if (pair? target)
				 target
				 (innermost-composition in: document
							at: cursor)))
		     (`(,name . ,_) target)
		     ((symbol? name))
		     (extension (extension name))
		     (illustration (invoke (as Extension extension)
					   'create-from target)))
	    (set! (origin illustration) target)
	    (replace-expression! at: cursor
				 with: illustration
				 in: document))))))
