(import
 (define-syntax-rule)
 (define-interface)
 (define-type)
 (define-object)
 (extent)
 (conversions)
 (indexable)
 (primitive)
 (text-screen)
 (combinators)
 (parse)
 (examples)
 (assert)
 (infix)
 (match)
 (term)
 (functions)
 (print)
 (screen)
 (for))

(define input ::string "
(define (factorial n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))
")

(define document ::list (parse-string input))


(define input-extent ::Extent (string-extent input))

(define cursor ::Cursor '())

(define-object (editor-message-handler size::int)::MessageHandler
  (define messages ::list '())

  (define history-length ::int)
  
  (define (add-message message::list)::void
    (let ((message-string (with-output-to-string
			    (lambda ()
			      (display "\n")
			      (for word in message
				(display word))
			      ))))
      (set! messages `(,message-string . ,messages))
      (drop-after! history-length messages)))
  
  (define (display-messages output::Object)::void
    (let ((io ::Terminal (as Terminal output)))
      (for message in messages
	(io:putString message))))

  (set! history-length size))

(define (run-editor #!optional (io ::Terminal (make-terminal)))::void
  (io:enterPrivateMode)
  (when (instance? io ExtendedTerminal)
    ((as ExtendedTerminal io):setMouseCaptureMode
     MouseCaptureMode:CLICK_RELEASE_DRAG))

  (parameterize ((current-message-handler (editor-message-handler 22))
		 (current-screen (TextScreen))
		 (current-display-procedure (lambda (message)
					      (io:putString
					       (with-output-to-string
						 (lambda ()
						   (display
						    message)))))))
    (let continue ()
      (let ((output-extent ::Extent (extent (head document))))
	(draw! (head document) cursor: cursor context: (recons 1 '()))
	(io:setCursorVisible #f)
	(io:clearScreen)
	(io:setCursorPosition 0 0)
	(io:putString ((current-screen):toString))
	(io:setCursorPosition 0 (+ 2 output-extent:height))
	(io:putString (with-output-to-string (lambda () (write cursor))))
	(try-catch
	 (io:putString (with-output-to-string
			 (lambda ()
			   (write (cursor-ref document cursor)))))
	 (ex java.lang.Throwable
	     (WARN (ex:toString))))
	(invoke (current-message-handler) 'display-messages io)
	(io:flush)
	(io:setCursorPosition (invoke (current-screen)
				      'remembered-left)
			      (+ 2 (invoke (current-screen)
					   'remembered-top)))
	(io:setCursorVisible #t)
	(let* ((key ::KeyStroke (io:readInput))
	       (type ::KeyType (key:getKeyType)))
	  (match type	
	    (,KeyType:ArrowLeft
	     (try-catch
	      (set! cursor (cursor-climb-back
			    (cursor-back cursor document)
			    document))
	      (ex java.lang.Throwable
		  (WARN (ex:toString))))
	     (continue))
	    
	    (,KeyType:ArrowRight
	     (try-catch
	      (begin
		(set! cursor (cursor-climb-front
			      (cursor-next cursor document)
			      document)))
	      (ex java.lang.Throwable
		  (WARN (ex:toString))))
	     (continue))
	    
	    (,KeyType:ArrowUp
	     (try-catch
	      (set! cursor (cursor-climb-front cursor document))
	      (ex java.lang.Throwable
		  (WARN (ex:toString))))
	     (continue))
	    
	    (,KeyType:ArrowDown
	     (try-catch
	      (set! cursor (cursor-climb-back cursor document))
	      (ex java.lang.Throwable
		  (WARN (ex:toString))))
	     (continue))
	    
	    (,KeyType:EOF
	     (values))

	    (,KeyType:Character
	     (continue))

	    (,KeyType:MouseEvent
	     (let* ((action ::MouseAction (as MouseAction key))
		    (position ::TerminalPosition (action:getPosition))
		    (left (position:getColumn))
		    (top (position:getRow)))
	       (cond ((action:isMouseMove)
		      (WARN "mouse move to " (position:toString)))
		     ((action:isMouseDown)
		      (match (action:getButton)
			(,MouseButton:Left
			 (try-catch
			  (let ((cursor (cursor-under left top
						      document)))
			    (WARN "cursor: " cursor))

			  (ex java.lang.Throwable
			      (WARN (ex:toString)))))
			(,MouseButton:Right
			 (WARN "right mouse at " (position:toString)))
			(_
			 (WARN (action:toString) " mouse at "
			       (position:toString)))))
		     ((action:isMouseDrag)
		      (WARN "mouse move to " (position:toString)))
		     ((action:isMouseUp)
		      (WARN "mouse released at "
			    (position:toString)))))
	     (continue))
	    
	    (_
	     (continue))
	    )
	  )))
    (io:exitPrivateMode)
    (io:close)))

(run-editor)
