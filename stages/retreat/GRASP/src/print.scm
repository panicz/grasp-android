(import (define-syntax-rule))
(import (define-interface))
(import (define-object))
(import (for))
(import (conversions))

(define-constant current-display-procedure::parameter
  (make-parameter display))

(define-syntax-rule (print elements ...)
  ((current-display-procedure) elements)
  ...
  ((current-display-procedure) #\newline))

(define-syntax-rule (dump expr)
  (print 'expr ": "expr))

(define-interface MessageHandler ()
  (clear-messages!)::void
  (add-message message::list)::void
  (display-messages output::Object)::void)

(define-object (default-message-handler)::MessageHandler
  (define last-message::string)

  (define (clear-messages!)::void
    (set! last-message #!null))
  
  (define (add-message message::list)::void
    (set! last-message
	  (with-output-to-string
	    (lambda ()
	      (for word in message
		(display word))
	      (newline)))))
  
  (define (display-messages output::Object)::void
    (display last-message)
    (set! last-message #!null)))

(define-constant current-message-handler::parameter[MessageHandler]
  (make-parameter (default-message-handler)))

(define (WARN . args)
  (invoke (current-message-handler) 'add-message args))
