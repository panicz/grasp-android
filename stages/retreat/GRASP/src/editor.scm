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
 )

(define input ::string "\
(define (factorial n)
  (if (<= n 0)
      1
      (* n (! (- n 1))))) 
(e.g. (factorial 5) ===> 120)
")

(define document ::list (cons (parse-string input) '()))

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

(define (delete! position::Index)::void
  (let* ((target (cursor-ref document cursor)))
    (cond
     ((is target instance? Symbol)
      (cond ((is 0 <= position < (symbol-length target))
	     (delete-char! target position)
	     (when (= (symbol-length target) 0)
	       (take-cell-at! (tail cursor) document)
	       (set! cursor (cursor-climb-back
			     (recons (- (head (tail cursor))
					1)
				     (tail (tail cursor)))
			     document))))))
     ((is target instance? Space)
      (if (is position > (first-index target))
	  (delete-space! target position))))))

(define (delete-forward!)::void
  (let ((target (cursor-ref document cursor)))
    (cond ((and (pair? target)
		(pair? cursor)
		(eqv? (head cursor) (first-index target)))
	   (let ((new-cursor (cursor-retreat cursor document)))
	     (take-cell-at! cursor document)
	     (set! cursor new-cursor)))
	  (else
	   (delete! (head cursor))))))

(define (delete-backward!)::void
  (let ((target (cursor-ref document cursor)))
    (cond ((and (pair? target)
		(eqv? (head cursor) (last-index target)))
	   (let ((new-cursor (cursor-climb-back
			      (cursor-back (tail cursor)
					   document)
			      document)))
	     (take-cell-at! cursor document)
	     (set! cursor new-cursor)))
	  (else
	   (set! cursor (cursor-climb-back
			 (cursor-back cursor
				      document)
			 document))
	   (delete! (head cursor))))))

(define (insert-character! c::char)::void
  (and-let* ((`(,tip . ,stem) cursor)
	     (`(,top . ,root) stem)
	     (parent (cursor-ref document root))
	     (owner (drop (quotient top 2) parent))
	     (target (part-at top parent)))
    (cond
     ((is c memq '(#\[ #\( #\{))
      (put-into-cell-at! (tail cursor) (cons '() '())
			 document)
      (set! cursor
	(recons* 0 0 (+ (head (tail cursor)) 1)
		 (tail (tail cursor)))))

     ((is c memq '(#\] #\) #\}))
      (WARN "closing paren"))
     
     ((is target instance? Symbol)
      (cond
       ((or (eq? c #\space) (eq? c #\newline))
	(cond ((eqv? (head cursor) (first-index target))
	       (let ((preceding-space (part-at
				       (previous-index
					top parent)
				       parent)))
		 (insert-whitespace! c preceding-space
				     (last-index
				      preceding-space))))
	      ((eqv? (head cursor) (last-index target))
	       (let ((following-space (part-at
				       (next-index
					top parent)
				       parent)))
		 (insert-whitespace! c following-space
				     (first-index
				      following-space))
		 (set! cursor
		   (cursor-advance cursor document))))
	      (else
	       (let* ((suffix (symbol-subpart target tip))
		      (cell (cons suffix (tail owner))))
		 (truncate-symbol! target tip)
		 (set! (tail owner) cell)
		 (set! (post-head-space cell)
		   (post-head-space owner))
		 (set! (post-head-space owner)
		   (Space fragments: (if (eq? c #\newline)
					 (cons 0
					       (cons 0 '()))
					 (cons 1 '()))))
		 (set! cursor
		   (cursor-advance cursor document))))))
       
	 (else
	  (insert-char! c target (head cursor))
	  (set! cursor
	    (recons (+ (head cursor) 1)
		    (tail cursor))))))
     ((is target instance? Space)

      (cond
       ((eq? c #\space)
	(insert-space! target (head cursor))
	(set! cursor (recons (+ (head cursor) 1)
			     (tail cursor)))
	)

       ((eq? c #\newline)
	(insert-break! target (head cursor)))
              
       (else
	(let* ((space-after (split-space!
			     target
			     (head cursor))))
	  (put-into-cell-at!
	   (tail cursor)
	   (cons (Symbol (list->string (list c)))
		 '())
	   document)
	  (set! cursor
	    (recons* 1 (+ (head (tail cursor)) 1)
		     (tail (tail cursor))))))))
     )))

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
	(draw-sequence! (head document)
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
		(set! cursor
		  (cursor-retreat cursor document)))
	      (ex java.lang.Throwable
		  (WARN (ex:toString))))
	     (continue))
	    
	    (,KeyType:ArrowRight
	     (try-catch
	      (begin
		(set! cursor
		  (cursor-advance cursor document))
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
	     (set! cursor (cursor-advance cursor document))
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
