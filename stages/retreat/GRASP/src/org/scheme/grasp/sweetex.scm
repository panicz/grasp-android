(import (srfi :17))

;; sweetex

(define-alias make-weak-key-hash-table java.util.WeakHashMap)
(define-alias make-hash-table java.util.HashMap)

;;(define-alias Pair gnu.lists.Pair)
;;(define-alias System java.lang.System)

(define (hash-set! table::java.util.Map key value)
  (table:put key value))

(define (hash-ref table::java.util.Map key . default)
  (if (table:contains-key key)
      (table:get key)
      (if (not (null? default))
	  ((car default)))))

(define (keyword->symbol kw)
  (string->symbol (keyword->string kw)))

(define (symbol->keyword s)
  (string->keyword (symbol->string s)))

(define-syntax define-syntax-rule 
  (syntax-rules ()
    ((define-syntax-rule (name . args) substitution)
     (define-syntax name
       (syntax-rules ()
	 ((name . args)
	  substitution))))))

(define-syntax-rule (assert proposition)
  (unless proposition (error "Assertion failed: " 'proposition)))

(define-syntax-rule (define-property (property-name object) default)
  (define property-name
    (let* ((override (make-weak-key-hash-table))
	   (getter (lambda (object)
		     (hash-ref override object
			       (lambda () default)))))
      (set! (setter getter) (lambda (arg value)
			      (hash-set! override arg value)))
      getter)))

(define-property (subclasses class) '())

(define-property (sealed? class) #false)

(define-syntax define-subtype
  (syntax-rules (either extend)
    ((_  supers type-name (either (variant . details) ...))
     (begin
       (define-simple-class type-name supers 
	 ((to-string)::string #!abstract)
	 ((equals object)::boolean #!abstract))
       (define-subtype (type-name) variant . details)
       ...
       ;;(set! (subclasses type-name) (list types ...))
       ;;(set! (sealed? type-name) #true)
       ))
    
    ((_ supers type-name (extend base . details))
     (begin
       ;;(assert (not (sealed? base)))
       (define-subtype (base . supers) type-name . details)))

    ((_ supers type-name . specs)
     (define-struct supers type-name . specs))
    ))
    
(define-syntax-rule (define-type type-name . details)
  (define-subtype () type-name . details))

(define-syntax-rule (define-struct supers type-name . specs)
  (struct-definition supers type-name
		     specs
		     ()
		     object
		     (instance? object type-name)
		     (symbol->string 'type-name)))


(define-syntax struct-definition
  (lambda (stx)
    (syntax-case stx ()
      ((_ supers type-name () slot-definitions
	  arg-name
	  equality-conditions
	  string-representation)
       #'(define-simple-class type-name supers
	   ((toString)::String
	    (string-append "[" string-representation "]"))
	   
	   ((equals arg-name)::boolean
	    equality-conditions)
	   
	   . slot-definitions))

      ((_ supers type-name
	  (slot-keyword slot-type =: value . rest)
	  (slot-definitions ...) arg-name
	  equality-conditions string-representation)
       (keyword? (syntax->datum #'slot-keyword))
       (with-syntax ((slot-symbol (datum->syntax
				   stx
				   (keyword->symbol
				    (syntax->datum #'slot-keyword)))))
	 #'(struct-definition
	    supers type-name rest
	    (slot-definitions ... (slot-symbol type: slot-type
					       init: value))
	    arg-name
	    (and equality-conditions
		 (equal? slot-symbol
			 (field arg-name 'slot-symbol)))
	    (string-append string-representation
			   " "(keyword->string 'slot-keyword)
			   ": "(java.lang.String:valueOf slot-symbol)))))
      ((_ supers type-name
	  (slot-keyword slot-type . rest)
	  (slot-definitions ...) arg-name
	  equality-conditions string-representation)
       (keyword? (syntax->datum #'slot-keyword))
       (with-syntax ((slot-symbol (datum->syntax
				   stx
				   (keyword->symbol
				    (syntax->datum #'slot-keyword)))))
	 #'(struct-definition
	    supers type-name rest
	    (slot-definitions ... (slot-symbol type: slot-type))
	    arg-name
	    (and equality-conditions
		 (equal? slot-symbol
			 (field arg-name 'slot-symbol)))
	    (string-append string-representation
			   " "(keyword->string 'slot-keyword)
			   ": "(java.lang.String:valueOf slot-symbol)))))
      )))

(define-syntax-rule (match expression (pattern actions* ... value) ...)
  (let ((evaluated expression))
    (match/evaluated evaluated (pattern actions* ... value) ...)))

(define-syntax match/evaluated
  (syntax-rules ()
    ((match/evaluated value)
     ;; This behavior is unspecified, and an "unspecified"
     ;; value would also be fine here.
     (error 'no-matching-pattern))

    ((match/evaluated value (pattern actions ...) . clauses)
     (match-clause ((pattern value))
                   (and)
                   ()
                   actions ...
                   (match/evaluated value . clauses)))))

(define-syntax match-clause
  (lambda (stx)
    (syntax-case stx (quasiquote unquote and _ %typename)
      ((match-clause () condition bindings actions ... alternative)
       #'(check/unique condition bindings #f () ()
		       actions ... alternative))

      ((match-clause ((`,pattern root) . rest)
                     condition
                     bindings
                     actions ... alternative)
       #'(match-clause ((pattern root) . rest)
                       condition
                       bindings
                       actions ... alternative))

      ((match-clause ((_ root) . rest)
                     condition
                     bindings
                     actions ... alternative)
       #'(match-clause rest
                       condition
                       bindings
                       actions ... alternative))

      ((match-clause ((variable root) . rest)
                     condition
                     bindings
                     actions ... alternative)
       (identifier? #'variable)
       #'(match-clause rest
                       condition
                       ((variable root) . bindings)
                       actions ... alternative))

      ((match-clause ((`(left . right) root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause ((`left (car root)) (`right (cdr root)) . rest)
                       (and conditions ... (pair? root))
                       bindings
                       actions ... alternative))

      ((match-clause (((_ . fields) root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause (((%typename . fields) root) . rest)
                       (and conditions ...)
                       bindings
                       actions ... alternative))
      
      ((match-clause (((%typename) root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause rest
                       (and conditions ...)
                       bindings
                       actions ... alternative))

      ((match-clause (((%typename key pat . etc) root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       (keyword? (syntax->datum #'key))
       (with-syntax ((name (datum->syntax
			    stx
			    (keyword->symbol (syntax->datum #'key)))))
	 #'(match-clause (((%typename . etc) root)
			  (pat (field root 'name)) . rest)
			 (and conditions ...)
			 bindings
			 actions ... alternative)))
    
      ((match-clause (((typename . fields) root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       (identifier? #'typename)
       #'(match-clause (((%typename . fields) root) . rest)
                       (and conditions ... (instance? root typename))
                       bindings
                       actions ... alternative))
      
      ((match-clause ((literal root) . rest)
                     (and conditions ...)
                     bindings
                     actions ...)
       #'(match-clause rest
                       (and conditions ... (equal? literal root))
                       bindings
                       actions ...))
      )))

(define-syntax check/unique
  (lambda (stx)
    "add equality checks for repeated identifiers in patterns and remove them from bindings"
    (syntax-case stx (and)
      ((check/unique condition () #f () bindings actions ... alternative)
       #'(if condition
             (let bindings actions ...)
             alternative))

      ((check/unique condition
                     ((variable path) . bindings)
                     #f
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       #'(check/unique condition
                       bindings
                       (variable path)
                       bindings/checked
                       bindings/final
                       actions ... alternative))

      ((check/unique (and conditions ...)
                     ((variable path) . bindings)
                     (variable+ path+)
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       (bound-identifier=? #'variable #'variable+)
       #'(check/unique (and conditions ... (equal? path path+))
                       bindings
                       (variable+ path+)
                       bindings/checked
                       bindings/final
                       actions ... alternative))

      ((check/unique conditions
                     ((variable path) . bindings)
                     (variable+ path+)
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       #'(check/unique conditions
                       bindings
                       (variable+ path+)
                       ((variable path) . bindings/checked)
                       bindings/final
                       actions ... alternative))

      ((check/unique conditions
                     ()
                     (variable path)
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       #'(check/unique conditions
                       bindings/checked
		       #f
		       ()
                       ((variable path) . bindings/final)
                       actions ... alternative))
      )))

(define-syntax-rule (define-interface name supers args ...)
  (interface-definition name supers (args ...) ()))

(define-syntax interface-definition
  (lambda (stx)
    (syntax-case stx (:: self)
      ((_ name supers () methods)
       #'(define-simple-class name supers interface: #true . methods))
      
      ((_ name supers (method :: result . rest) (methods ...))
       #'(interface-definition
	  name supers rest (methods ... (method :: result #!abstract))))
      )))

;;(define-alias canvas android.graphics.Canvas)

(define-interface tile ()
  (width)::int
  (height)::int
  (try-set-size! w::int h::int)::void
  ;;(render target::canvas)::void
  )

(define (width x::tile)::int
  (invoke x 'width))

(define (height x::tile)::int
  (invoke x 'height))
