(import
 (define-syntax-rule)
 (define-interface)
 (define-type)
 (define-object)
 (extent)
 (conversions)
 (indexable)
 (space)
 (cursor)
 (tile)
 (primitive)
 (extent)
 (text-screen)
 (combinators)
 (parse)
 (symbol)
 (examples)
 (assert)
 (infix)
 (match)
 (term)
 (functions)
 (print)
 (screen)
 (for)
 (document-operations)
 (editor-operations)
 )

(define input ::string "\
(define (! n)
  (if (<= n 0)
      1
      (* n (! (- n 1))))) 
(e.g. (factorial 5) ===> 120)
")

(set! (the-document)
  (with-input-from-string input parse-document))

(define input-extent ::Extent (string-extent input))

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
		  (editor-message-handler 6))
		 (the-screen (TextScreen))
		 (current-display-procedure
		  (lambda (message)
		    (io:putString
		     (with-output-to-string
		       (lambda ()
			 (display
			  message)))))))
          
    (let continue ()
      (let ((output-extent ::Extent
			   (extent (head (the-document)))))
	((the-screen):clear!)
	(draw-sequence! (head (the-document)))
	(io:setCursorVisible #f)
	(io:clearScreen)
	(io:setCursorPosition 0 0)
	(io:putString ((the-screen):toString))
	(io:setCursorPosition
	 0
	 (+ 2 output-extent:height))
	(io:putString (with-output-to-string
			(lambda () (write (the-cursor)))))
	(try-catch
	 (io:putString (with-output-to-string
			 (lambda ()
			   (write (the-expression)))))
	 (ex java.lang.Throwable
	     (WARN (ex:toString))))
	(invoke (current-message-handler)
		'display-messages io)
	(io:flush)
	(io:setCursorPosition
	 (invoke (the-screen)
		 'remembered-left)
	 (invoke (the-screen)
		 'remembered-top))
	(io:setCursorVisible #t)
	(let* ((key ::KeyStroke (io:readInput))
	       (type ::KeyType (key:getKeyType)))
	  (match type	
	    (,KeyType:ArrowLeft
	     (try-catch
	      (begin
		(cursor-retreat!))
	      (ex java.lang.Throwable
		  (WARN (ex:toString))))
	     (continue))
	    
	    (,KeyType:ArrowRight
	     (try-catch
	      (begin
		(cursor-advance!)
		(cond ((key:shift-down?)
		       "powiekszamy selekcje")
		      
		      ((key:ctrl-down?)
		       "przeskakujemy atomy")))
	      (ex java.lang.Throwable
		  (WARN (ex:toString))))
	     (continue))
	    
	    (,KeyType:ArrowUp
	     (try-catch
	      (invoke (the-screen)
		      'remember-offset!
		      (invoke (the-screen)
			      'remembered-left)
		      (- (invoke (the-screen)
				 'remembered-top)
			 1))
	      (ex java.lang.Throwable
		  (WARN (ex:toString))))
	     (continue))
	    
	    (,KeyType:ArrowDown
	     (try-catch
	      (invoke (the-screen)
		      'remember-offset!
		      (invoke (the-screen)
			      'remembered-left)
		      (+ (invoke (the-screen)
				 'remembered-top)
			 1))
	      (ex java.lang.Throwable
		  (WARN (ex:toString))))
	     (continue))
	    
	    (,KeyType:EOF
	     (values))

	    (,KeyType:Character
	     
	     (let* ((code::char
		     ((key:getCharacter):charValue))
		    (c::gnu.text.Char (gnu.text.Char
				       code)))
	       (if (and (key:ctrl-down?)
			(eq? c #\space))
		   (invoke (current-message-handler)
			   'clear-messages!)
		   (insert-character! c)
		   )
	       (continue)))

	    (,KeyType:Delete
	     (try-catch
	      (delete-forward!)
	      (ex java.lang.Throwable
		  (WARN (ex:toString))))
	     (continue))

	    (,KeyType:Enter
	     (insert-character! #\newline)
	     (cursor-advance!)
	     (continue))
	    
	    (,KeyType:Backspace
	     (try-catch
	      (delete-backward!)
	      (ex java.lang.Throwable
		  (WARN (ex:toString))))

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
		    (invoke (the-screen)
			    'remember-offset!
			    left top)
		    (try-catch
		     (let ((cursor+
			    (cursor-under
			     left top
			     (head (the-document)))))
		       (set! (the-cursor) cursor+)
		       (WARN "cursor: "
			     (the-cursor)))

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
