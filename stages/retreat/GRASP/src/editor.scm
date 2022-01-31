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
    (io:flush)
    (let* ((key ::KeyStroke (io:readInput))
	   (type ::KeyType (key:getKeyType)))
             
      (match type	
	(,KeyType:ArrowLeft
	 (try-catch
	  (set! cursor (cursor-climb-back
			(cursor-back cursor document)
			document))
	  (ex java.lang.Throwable
	      (io:putString (ex:toString))))
	 (continue))
	
	(,KeyType:ArrowRight
	 (try-catch
	  (set! cursor (cursor-climb-front
			(cursor-next cursor document)
			document))
	  (ex java.lang.Throwable
	      (io:putString (ex:toString))))
	 (continue))
	
	(,KeyType:ArrowUp
	 (try-catch
	  (set! cursor (cursor-climb-front cursor document))
	  (ex java.lang.Throwable
	      (io:putString (ex:toString))))
	 (continue))
	
	(,KeyType:ArrowDown
	 (try-catch
	  (set! cursor (cursor-climb-back cursor document))
	  (ex java.lang.Throwable
	      (io:putString (ex:toString))))
	 (continue))
	
	(,KeyType:EOF
	 (values))

	(,KeyType:Character
	 (continue))
	
	(_
	 (continue))
	)
      ))
  (io:exitPrivateMode)
  (io:close))

(run-editor)
