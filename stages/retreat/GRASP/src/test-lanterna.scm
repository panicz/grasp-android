(import (term))
(import (for))

(define io ::Terminal (make-terminal))
;;(define ui ::TerminalScreen (TerminalScreen io))

(io:setBackgroundColor TextColor:ANSI:RED)
(io:putCharacter #\space)
(io:setBackgroundColor TextColor:ANSI:YELLOW)
(io:putCharacter #\space)
(io:setBackgroundColor TextColor:ANSI:GREEN)
(io:putCharacter #\space)
(io:setBackgroundColor TextColor:ANSI:CYAN)
(io:putCharacter #\space)
(io:setBackgroundColor TextColor:ANSI:BLUE)
(io:putCharacter #\space)
(io:setBackgroundColor TextColor:ANSI:MAGENTA)
(io:putCharacter #\space)


(io:setBackgroundColor TextColor:ANSI:RED_BRIGHT)
(io:putCharacter #\space)
(io:setBackgroundColor TextColor:ANSI:YELLOW_BRIGHT)
(io:putCharacter #\space)
(io:setBackgroundColor TextColor:ANSI:GREEN_BRIGHT)
(io:putCharacter #\space)
(io:setBackgroundColor TextColor:ANSI:CYAN_BRIGHT)
(io:putCharacter #\space)
(io:setBackgroundColor TextColor:ANSI:BLUE_BRIGHT)
(io:putCharacter #\space)
(io:setBackgroundColor TextColor:ANSI:MAGENTA_BRIGHT)
(io:putCharacter #\space)

(io:setBackgroundColor TextColor:ANSI:DEFAULT)
(io:putCharacter #\newline)


(io:setBackgroundColor TextColor:ANSI:RED)
(io:putCharacter #\space)
(io:setBackgroundColor TextColor:ANSI:RED_BRIGHT)
(io:putCharacter #\space)
(io:setBackgroundColor TextColor:ANSI:YELLOW)
(io:putCharacter #\space)

(io:setBackgroundColor TextColor:ANSI:YELLOW_BRIGHT)
(io:putCharacter #\space)

;;(io:setBackgroundColor TextColor:ANSI:GREEN)
;;(io:putCharacter #\space)
(io:setBackgroundColor TextColor:ANSI:GREEN_BRIGHT)
(io:putCharacter #\space)
(io:setBackgroundColor TextColor:ANSI:CYAN_BRIGHT)
(io:putCharacter #\space)

(io:setBackgroundColor TextColor:ANSI:CYAN)
(io:putCharacter #\space)
(io:setBackgroundColor TextColor:ANSI:BLUE)
(io:putCharacter #\space)
(io:setBackgroundColor TextColor:ANSI:BLUE_BRIGHT)
(io:putCharacter #\space)
(io:setBackgroundColor TextColor:ANSI:MAGENTA)
(io:putCharacter #\space)

;;(io:setBackgroundColor TextColor:ANSI:MAGENTA_BRIGHT)


(io:setBackgroundColor TextColor:ANSI:DEFAULT)
(io:putCharacter #\newline)




(for r from 0 to 255 by 51
     (for g from 0 to 255 by 51
	  (for b from 0 to 255 by 51
	       (io:setBackgroundColor (TextColor:RGB r g b))
	       (io:putCharacter #\space)))
	  (io:setBackgroundColor TextColor:ANSI:DEFAULT)
	  (io:putCharacter #\newline))
	  
