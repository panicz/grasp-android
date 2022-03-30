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

(define-object (editor-message-handler size::int)
  ::MessageHandler
  (define messages ::list '())

  (define history-length ::int)

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
      (drop-after! history-length messages)))
  
  (define (display-messages output::Object)::void
    (let ((io ::Terminal (as Terminal output)))
      (for message in messages
	(io:putString message))))

  (set! history-length size))

(define (run-editor #!optional
		    (io ::Terminal (make-terminal)))
  ::void
  (io:enterPrivateMode)
  (when (instance? io ExtendedTerminal)
    (invoke (as ExtendedTerminal io)
	    'setMouseCaptureMode
	    MouseCaptureMode:CLICK_RELEASE_DRAG))

  (parameterize ((current-message-handler
		  (editor-message-handler 16))
		 (current-screen (TextScreen))
		 (current-display-procedure
		  (lambda (message)
		    (io:putString
		     (with-output-to-string
		       (lambda ()
			 (display
			  message)))))))
    (let continue ()
      (let ((output-extent ::Extent
			   (extent (head document)))
	    (top-cursor (recons 1 '())))
	((current-screen):clear!)
	(draw! (head document)
	       cursor: cursor
	       context: top-cursor)
	(io:setCursorVisible #f)
	(io:clearScreen)
	(io:setCursorPosition 0 0)
	(io:putString ((current-screen):toString))
	(io:setCursorPosition
	 0
	 (+ 2 output-extent:height))
	(io:putString (with-output-to-string
			(lambda () (write cursor))))
	(try-catch
	 (io:putString (with-output-to-string
			 (lambda ()
			   (write (cursor-ref
				   document
				   cursor)))))
	 (ex java.lang.Throwable
	     (WARN (ex:toString))))
	(invoke (current-message-handler)
		'display-messages io)
	(io:flush)
	(io:setCursorPosition
	 (invoke (current-screen)
		 'remembered-left)
	 (invoke (current-screen)
		 'remembered-top))
	(io:setCursorVisible #t)
	(let* ((key ::KeyStroke (io:readInput))
	       (type ::KeyType (key:getKeyType)))
	  (match type	
	    (,KeyType:ArrowLeft
	     (try-catch
	      (begin
		(set! cursor (cursor-climb-back
			      (cursor-back cursor
					   document)
			      document)))
	      (ex java.lang.Throwable
		  (WARN (ex:toString))))
	     (continue))
	    
	    (,KeyType:ArrowRight
	     (try-catch
	      (begin
		(set! cursor (cursor-climb-front
			      (cursor-next cursor
					   document)
			      document))
		(cond ((key:shift-down?)
		       "powiekszamy selekcje")
		      
		      ((key:ctrl-down?)
		       "przeskakujemy atomy")))
	      (ex java.lang.Throwable
		  (WARN (ex:toString))))
	     (continue))
	    
	    (,KeyType:ArrowUp
	     (try-catch
	      (invoke (current-screen)
		      'remember-offset!
		      (invoke (current-screen)
			      'remembered-left)
		      (- (invoke (current-screen)
				 'remembered-top)
			 1))
	      (ex java.lang.Throwable
		  (WARN (ex:toString))))
	     (continue))
	    
	    (,KeyType:ArrowDown
	     (try-catch
	      (invoke (current-screen)
		      'remember-offset!
		      (invoke (current-screen)
			      'remembered-left)
		      (+ (invoke (current-screen)
				 'remembered-top)
			 1))
	      (ex java.lang.Throwable
		  (WARN (ex:toString))))
	     (continue))
	    
	    (,KeyType:EOF
	     (values))

	    (,KeyType:Character
	     
	     (let ((c::Character (key:getCharacter)))
	       ;; no dobra, to tutaj bedziemy mieli kilka
	       ;; zagwozdek. zasadniczo to chyba bysmy
	       ;; probowali wysylac wcisniety klawisz
	       ;; do naszego obiektu. Ale w jaki sposob
	       ;; mozemy to zrobic?
	       ;; Wydaje sie, ze sensowny bylby taki interfejs:
	       ;; (send-character! jakis-znak kursor poziom)
	       (if (and (key:ctrl-down?)
			(eq? (#\space:charValue) (c:charValue)))
		   (invoke (current-message-handler)
			   'clear-messages!)
		   (set! cursor
			 (send-char-to! document (c:charValue)
					cursor))
		   )
		(continue)))

	    (,KeyType:Delete
	     (set! cursor
		   (send-char-to! document #\delete
				  cursor))
	     (continue))

	    (,KeyType:Backspace
	     (set! cursor
		   (send-char-to! document #\backspace
				  cursor))
	     (continue))

	    (,KeyType:Enter
	     (set! cursor
		   (send-char-to! document #\newline
				  cursor))
	     (continue))
	    
	    (,KeyType:MouseEvent
	     (let* ((action ::MouseAction
			    (as MouseAction key))
		    (position ::TerminalPosition
			      (action:getPosition))
		    (left (position:getColumn))
		    (top (position:getRow)))
	       (cond
		((action:isMouseMove)
		 (WARN "mouse move to "
		       (position:toString)))
		((action:isMouseDown)
		 (match (action:getButton)
		   (,MouseButton:Left
		    (invoke (current-screen)
			    'remember-offset!
			    left top)
		    (try-catch
		     (let ((cursor+
			    (cursor-under
			     left
			     top
			     (head document)
			     context:
			     top-cursor
			     )))
		       (set! cursor cursor+)
		       (WARN "cursor: "
			     cursor))

		     (ex java.lang.Throwable
			 (WARN (ex:toString)))))
		   (,MouseButton:Right
		    (WARN "right mouse at "
			  (position:toString)))
		   (_
		    (WARN (action:toString)
			  " mouse at "
			  (position:toString)))))
		((action:isMouseDrag)
		 (WARN "mouse move to "
		       (position:toString)))
		((action:isMouseUp)
		 (WARN "mouse released at "
		       (position:toString)))))
	     (continue))
	    
	    (_
	     (WARN "key: "key)
	     (continue))
	    )
	  )))
    (io:exitPrivateMode)
    (io:close)))

(run-editor)
