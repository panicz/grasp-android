(import (match))
(import (infix))

(define-alias DefaultTerminalFactory
  com.googlecode.lanterna.terminal.DefaultTerminalFactory)

(define-alias TerminalPosition
  com.googlecode.lanterna.TerminalPosition)

(define-alias TerminalSize
  com.googlecode.lanterna.TerminalSize)

(define-alias TextColor com.googlecode.lanterna.TextColor)

(define-alias Terminal com.googlecode.lanterna.terminal.Terminal)

(define-alias KeyStroke com.googlecode.lanterna.input.KeyStroke)

(define-alias KeyType com.googlecode.lanterna.input.KeyType)

(define defaultTerminalFactory
  ::DefaultTerminalFactory (DefaultTerminalFactory))

(define (make-terminal)::Terminal
  (defaultTerminalFactory:createTerminal))

(define (run-editor #!optional (io ::Terminal (make-terminal)))::void
  (io:enterPrivateMode)
  (io:setCursorPosition 0 0)
  (io:clearScreen)
  (let continue ()
    (io:flush)
    (let* ((key ::KeyStroke (io:readInput))
	   (position ::TerminalPosition (io:getCursorPosition))
	   (size ::TerminalSize (io:getTerminalSize))
	   (row ::int (position:getRow))
	   (col ::int (position:getColumn))
	   (width ::int (size:getColumns))
	   (height ::int (size:getRows))
	   (type ::KeyType (key:getKeyType)))
      ;;(io:setCursorPosition 0 0)
      ;;(io:putString (key:toString))
      (match type	
	(,KeyType:ArrowLeft
	 (io:setCursorPosition (max 0 (- col 1)) row)
	 (continue))
	
	(,KeyType:ArrowRight
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
