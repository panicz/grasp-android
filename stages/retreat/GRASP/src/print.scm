(import (define-syntax-rule))
(import (define-interface))
(import (define-object))
(import (for))
(import (conversions))
(import (functions))

(define-constant current-display-procedure::parameter
  (make-parameter display))

(define-syntax-rule (print elements ...)
  ((current-display-procedure) elements)
  ...
  ((current-display-procedure) #\newline)
  (force-output))

(define-interface MessageHandler ()
  (clear-messages!)::void
  (add-message message::list)::void
  (display-messages output::Object)::void)

(define-object (logger size::int)::MessageHandler
  (define messages ::list '())

  (define historyLength ::int)

  (define (clear-messages!)::void
    (set! messages '()))
  
  (define (add-message message::list)::void
    (let ((message-string (with-output-to-string
			    (lambda ()
			      (display "\n")
			      (for word in message
				(display word))
			      ))))
      (set! messages `(,message-string . ,messages))
      (drop-after! historyLength messages)))
  
  (define (display-messages output::Object)::void
    #!abstract)

  (set! historyLength size))


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

(define-syntax-rule (DUMP expr ...)
  (print 'expr ": "expr)
  ...)

(define-syntax-rule (truly actions ...)
  (begin actions ... #t))

(define-syntax-rule (falsely actions ...)
  (begin actions ... #f))
