(import (define-interface))
(import (mapping))
(import (tile))
(import (primitive))
(import (match))
(import (functions))
(import (cursor))
(import (document-operations))
(import (print))

(define-interface Enchanted (Tile)
  ;; by convention, the return value of #true means
  ;; that the event was handled by the object,
  ;; and the value of #false means that it was ignored
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

  (methods)::list
  
  (s-expression)::cons
  )

(define-interface Extension ()
  (create-from source::cons)::Enchanted
  )

(define-mapping (extension keyword)
  (begin
    (WARN "no extension for "keyword)
    #f))

(define (enchant-expression! #!optional
			     #;at (cursor::Cursor (the-cursor))
				  #;in (document (the-document)))
  (let ((target (innermost-composition in: document at: cursor)))
    (if (Enchanted? target)
	(let ((original (invoke (as Enchanted target)
				's-expression)))
	  (replace-expression! at: cursor
			       with: original
			       in: document))
	(and-let* ((`(,name . ,_) target)
		   ((symbol? name))
		   (extension (extension name))
		   (illustration (invoke (as Extension extension)
					 'create-from target)))
	  (replace-expression! at: cursor
			       with: illustration
			       in: document)))))

#|

(define-extension (Graph vertices: list
			 edges: list)
  ...)

(define (ignore . _) #f)

(define (absorb . _) #t)

(define-object (KeyMappableIllustration)::Enchanted
  (define key-press :: Map (mapping (key) ignore))
  (define key-release :: Map (mapping (key) ignore))

  (define (key-pressed key::char)
    (key-press key))

  (define (key-released key::char)
    (key-release key))
  
  (define (set-on-key-press! key::char action::procedure)
    (set! (key-press key) action))

  (define (set-on-key-release! key::char action::procedure)
    (set! (key-release key) action))
  
  (define (methods)::list
    `((set-on-key-press! . ,(lambda (self::KeyMappableIllustration
				     key::char
				     action::procedure)
			      (self:set-on-key-press! key action)))
      (set-on-key-release! . ,(lambda (self::KeyMappableIllustration
				       key::char
				       action::procedure)
				(self:set-on-key-release!
				 key action)))))

  (define (s-expression)::cons #!abstract)

  ...)


(define-type
  (Button content: Tile
	  on-tap: (Button x y -> boolean) := configure-button
	  on-double-tap: (Button real real -> boolean) := ignore
	  on-press: (Button real real -> boolean)
	  )
  implementing Enchanted
  with
  ((key-pressed key::char)::boolean
   (on-key-press (this) key))

  ((key-released key::char)::boolean
   (on-key-release (this) key))
  
  ((tapped x::real y::real)::boolean
   (on-tap (this) x y))
   
  ((pressed x::real y::real)::boolean
   (on-press (this) x y))
  
  ((released x::real y::real)::boolean
   (on-release (this) x y))
  
  ((dragged-over x::real y::real item::Tile*)::boolean
   (on-drag-over (this) x y item))
  
  ((dragged-out x::real y::real item::Tile*)::boolean
   (on-drag-out (this) x y item))
  
  ((dropped x::real y::real item::Tile*)::boolean
   (on-drop (this) x y item))
  
  ((held x::real y::real)::boolean
   (on-hold (this) x y))
   
  ((double-tapped x::real y::real)::boolean
   (on-double-tap (this) x y))
   
  ((second-pressed x::real y::real)::boolean
   (on-second-press (this) x y))
  
  )

		     
  
			  
			  

(Button
 content: (Label "Hello")
 on-tap: (lambda (self x y)
	   
  
  


(define-syntax define-extension
  (lambda (stx)
    (syntax-case stx ()
      ((_ (<extension-name> . args) . spec)
       (with-syntax ((<extension-name>-extension
		      (extension-name <extension-name>)))
	 #'(begin
	     (define-class <extension-name>
	       (Enchanted)
	       ...)
	     (define-class construction-class (Extension)
	       ((create source::cons)
		
	       ...)
	     (set! (extension '<extension-name>)
		   (<extension-name>-extension))
	     ...)
|#
