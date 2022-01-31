(import
 (cell-display-properties)
 (define-interface)
 (define-type)
 (extent)
 (conversions)
 (primitive)
 (text-screen)
 (combinators)
 (parse)
 (examples)
 (assert)
 (cursor)
 (infix)
 (match)
 (term))

(define input ::string "
(define (factorial n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))
")

(define document ::list (parse-string input))

;; powinien byc raczej TerminalScreen
(set! (current-screen) (TextScreen))

(define input-extent ::Extent (string-extent input))

(define output-extent ::Extent (draw! (head document)))

(define cursor ::Cursor '())

(define (run-editor #!optional (io ::Terminal (make-terminal)))::void
  (io:enterPrivateMode)
  (when (instance? io ExtendedTerminal)
    ((as ExtendedTerminal io):setMouseCaptureMode
     MouseCaptureMode:CLICK_RELEASE_DRAG))
  (let ((warning ""))
    (let continue ()
      (io:clearScreen)
      (io:setCursorPosition 0 0)
      (io:putString ((current-screen):toString))
      (io:setCursorPosition 0 (+ 2 output-extent:height))
      (io:putString (with-output-to-string (lambda () (write cursor))))
      (io:setCursorPosition 0 (+ 4 output-extent:height))
      (try-catch
       (begin
	 (io:putString (show->string (cursor-ref document cursor))))
       (ex java.lang.Throwable
	   (io:putString (ex:toString))))
      (io:putString warning)
      (io:flush)
      (set! warning "")
      (let* ((key ::KeyStroke (io:readInput))
	     (type ::KeyType (key:getKeyType)))
        
	(match type	
	  (,KeyType:ArrowLeft
	   (try-catch
	    (set! cursor (cursor-climb-back
			  (cursor-back cursor document)
			  document))
	    (ex java.lang.Throwable
		(set! warning (ex:toString))))
	   (continue))
	  
	  (,KeyType:ArrowRight
	   (try-catch
	    (set! cursor (cursor-climb-front
			  (cursor-next cursor document)
			  document))
	    (ex java.lang.Throwable
		(set! warning (ex:toString))))
	   (continue))
	  
	  (,KeyType:ArrowUp
	   (try-catch
	    (set! cursor (cursor-climb-front cursor document))
	    (ex java.lang.Throwable
		(set! warning (ex:toString))))
	   (continue))
	  
	  (,KeyType:ArrowDown
	   (try-catch
	    (set! cursor (cursor-climb-back cursor document))
	    (ex java.lang.Throwable
		(set! warning (ex:toString))))
	   (continue))
	  
	  (,KeyType:EOF
	   (values))

	  (,KeyType:Character
	   (continue))

	  (,KeyType:MouseEvent
	   (let* ((action ::MouseAction (as MouseAction key))
		  (position ::TerminalPosition (action:getPosition)))
	     (cond ((action:isMouseMove)
		    (set! warning
			  (string-append "mouse move to "
					 (position:toString))))
		   ((action:isMouseDown)
		    (match (action:getButton)
		      (,MouseButton:Left
		       (set! warning
			     (string-append "left mouse at "
					    (position:toString))))
		      (,MouseButton:Right
		       (set! warning
			     (string-append "right mouse at "
					    (position:toString))))
		      (_
		       (set! warning
			     (string-append (action:toString)
					    " mouse at "
					    (position:toString))))))
		   ((action:isMouseDrag)
		    (set! warning
			  (string-append "mouse move to "
					 (position:toString))))
		   ((action:isMouseUp)
		    (set! warning
			  (string-append "mouse released at "
					 (position:toString))))))
	   (continue))
	  
	  (_
	   (continue))
	  )
	))
    (io:exitPrivateMode)
    (io:close)))

(run-editor)
