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

