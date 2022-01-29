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

(define input-extent ::Extent (string-extent input))

(define cursor ::Cursor '())

(define (run-editor #!optional (io ::Terminal (make-terminal)))::void
  (io:enterPrivateMode)
  (io:setCursorPosition 0 0)
  (io:clearScreen)
  (let continue ()
    (let* ((key ::KeyStroke (io:readInput))
	   (position ::TerminalPosition (io:getCursorPosition))
	   (size ::TerminalSize (io:getTerminalSize))
	   (row ::int (position:getRow))
	   (col ::int (position:getColumn))
	   (width ::int (size:getColumns))
	   (height ::int (size:getRows))
	   (type ::KeyType (key:getKeyType)))

      (io:setCursorPosition 0 0)
      (io:putString (show->string document))
      (io:setCursorPosition 0 (+ 2 input-extent:height))
      (io:putString (show->string cursor))
      (io:setCursorPosition 0 (+ 4 input-extent:height))
      
      (try-catch
       (begin
	 (io:putString (show->string (cursor-ref document cursor))))
       (ex java.lang.Throwable
	   (io:putString (ex:toString))))
       
      (io:flush)
      
      (match type	
	(,KeyType:ArrowLeft
	 (try-catch
	  (set! cursor (cursor-climb-back (cursor-back cursor document) document))
	  (ex java.lang.Throwable
	      (io:putString (ex:toString))))
	 (io:setCursorPosition (max 0 (- col 1)) row)
	 (continue))
	
	(,KeyType:ArrowRight
	 (try-catch
	  (set! cursor (cursor-climb-front (cursor-next cursor document) document))
	  (ex java.lang.Throwable
	      (io:putString (ex:toString))))

	 (io:setCursorPosition (min (+ col 1) (- width 1)) row)
	 (continue))
	
	(,KeyType:ArrowUp
	 (io:setCursorPosition col (max 0 (- row 1)))
	 (continue))
	
	(,KeyType:ArrowDown
	 (io:setCursorPosition col (min (+ row 1) (- height 1)))
	 (continue))
	
	(,KeyType:EOF
	 (values))

	(,KeyType:Character
	 ;;(io:setCursorPosition 0 1)
	 ;;(io:putCharacter (key:getCharacter))
	 (io:setCursorPosition col row)
	 (continue))
	
	(_
	 (io:setCursorPosition col row)
	 (continue))
	)))
  (io:exitPrivateMode)
  (io:close))

(run-editor)
