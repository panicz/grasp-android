(define-alias DefaultTerminalFactory
  com.googlecode.lanterna.terminal.DefaultTerminalFactory)

(define-alias TerminalPosition
  com.googlecode.lanterna.TerminalPosition)

(define-alias TextColor com.googlecode.lanterna.TextColor)

(define-alias Terminal com.googlecode.lanterna.terminal.Terminal)

(define-alias KeyStroke com.googlecode.lanterna.input.KeyStroke)

(define-alias KeyType com.googlecode.lanterna.input.KeyType)

(define defaultTerminalFactory
  ::DefaultTerminalFactory (DefaultTerminalFactory))

(define (make-terminal)::Terminal
  (defaultTerminalFactory:createTerminal))

#|
(define (run-editor #!optional (io (make-terminal)))
  (let* ((key ::KeyStroke (io:read-input))
	 (position ::TerminalPosition (io:getCursorPositiin))
	 (type ::KeyType (key:getKeyType)))
    (match type
      (KeyType:Character
       ...)
      (KeyType:ArrowLeft
       ...)
      (KeyType:ArrowRight
       ...)
      (KeyType:ArrowUp
       ...)
      (KeyType:ArrowDown
       ...)
      (KeyType:Escape
       ...)
      )))
  |#    
