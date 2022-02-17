(import
 (define-syntax-rule)
 (define-interface)
 (define-type)
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
 (screen))

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
    (define-syntax-rule (WARN msg ...)
      (set! warning (string-append "\n" msg ... warning)))
    
    (let continue ()
      (io:setCursorVisible #f)
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
	   (WARN (ex:toString))))
      (io:putString warning)
      (io:flush)
      (io:setCursorVisible #t)
      ;;(set! warning "")
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
		  (position ::TerminalPosition (action:getPosition)))
	     (cond ((action:isMouseMove)
		    (WARN "mouse move to " (position:toString)))
		   ((action:isMouseDown)
		    (match (action:getButton)
		      (,MouseButton:Left
		       (WARN "left mouse at " (position:toString)))
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
	))
    (io:exitPrivateMode)
    (io:close)))

(run-editor)
