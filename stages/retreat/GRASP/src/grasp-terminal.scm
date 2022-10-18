(module-name grasp-terminal)
(module-compile-options main: #t)

(import (define-syntax-rule))
(import (define-interface))
(import (define-property))
(import (define-type))
(import (define-object))
(import (define-cache))
(import (default-value))
(import (define-parameter))
(import (extent))
(import (fundamental))
;;(import (conversions))
;;(import (indexable))
;;(import (space))
(import (cursor))
(import (primitive))
(import (extent))
;;(import (combinators))
(import (parse))
(import (examples))
(import (assert))
(import (infix))
(import (match))
(import (term))
(import (functions))
(import (print))
(import (painter))
(import (for))
(import (while))
(import (document-operations))
(import (editor-operations))
(import (text-painter))
;;(import (extension))
;;(import (button))
(import (input))
(import (panel))



(define-alias Thread java.lang.Thread)

;; OK, wyglada na to, ze klienta terminalowego
;; bedziemy rozwijac na telefonie.

;; nie zmienia to jednak faktu, ze chcemy
;; sie teraz zajac klikaniem

;; plan zatem taki:
;; - dodajemy metode cursor-under* do interfejsu Element
;; - tworzymy implementacje tej metody dla:
;;   - spacji
;;   - textu
;;   - atomow
;;   - par/sekwencji
;;   - kombinacji
;;   - innych instancji?
;; - tworzymy duzo testow jednostkowych
;;   (moze byc w test-painter)

;; dalej: poki siedzimy na telefonie, powinnismy
;; rozbudowac klienta terminalowego
;; (tak zeby finalnie usunac primitive-terminal-client)
;;
;; fajnie by tez bylo zaimplementowac protokol
;; kitty do rysowania w terminalu
;; albo terminal ze wsparciem dla grafiki wektorowej
;;
;; ale teraz to bez znaczenia
;;
;; 

(define-syntax define-box
  (syntax-rules (::)
    ((_ (name)::type initial-value)
     (define-constant name 
       (let* ((state ::type initial-value)
	      (getter (lambda () state)))
	 (set! (setter getter) (lambda (value::type) (set! state value)))
	 getter)))
       
    ((_ (name) initial-value)
     (define-constant name 
       (let* ((state initial-value)
	      (getter (lambda () state)))
	 (set! (setter getter) (lambda (value) (set! state value)))
	 getter)))
    ))

(define-box (screen-up-to-date?)::boolean #f)

(define-parameter (the-text-style)::TextStyle
  (TextStyle:noneOf TextDecoration:class))

(define-parameter (the-text-color)::Color
  Color:ANSI:DEFAULT)

(define-parameter (the-background-color)::Color
  Color:ANSI:DEFAULT)

(define-cache (letter character
		      color: color::Color := (the-text-color)
		      background: background::Color
		      := (the-background-color)
		      style: style::TextStyle := (the-text-style))
  ::Letter
   (Letter character color background style))

(define (render io :: LanternaScreen)::void
  (synchronized screen-up-to-date?
		(while (screen-up-to-date?)
		  (invoke screen-up-to-date? 'wait))
		;; if - during rendering - the state of
		;; the panel changes (by the editing thread),
		;; then this value will be set to #f, and
		;; another iteration of the rendering loop
		;; will be forced.
		;; (This idea was inspired by Richard Stallman's
		;; 1981 paper "EMACS: The Extensible,
		;; Customizable Display Editor", section 13
		;; "The Display Processor")
		(set! (screen-up-to-date?) #t))
  ;; tutaj chcemy sobie wywolac funkcje rysujaca
  ;; aczkolwiek na poczatek wystarczy po prostu rysowac
  ;; pojedynczy znaczek, przesuwany za pomoca klawiszy strzalek,
  ;; albo cos takiego
  (let* ((resize ::TerminalSize (io:doResizeIfNecessary))
	 (size (or resize (io:getTerminalSize)))
	 (extent ::Extent (the-screen-extent))
	 (painter (the-painter)))
    (set! extent:width (size:getColumns))
    (set! extent:height (size:getRows))
    (painter:clear!)
    (invoke (the-top-panel) 'draw! ())
    ;; swap front- and back-buffer
    (io:refresh (if resize
		    LanternaScreen:RefreshType:COMPLETE
		    LanternaScreen:RefreshType:DELTA))
    (render io)))

(define (edit io :: LanternaScreen)::void
  (let* ((key ::KeyStroke (io:readInput))
	 (type ::KeyType (key:getKeyType))
	 (caret ::TerminalPosition (io:getCursorPosition)))

    ;; powinnismy sobie wymyslic jakas warstwe abstrakcji,
    ;; ktora obslugiwalaby klawisze:
    ;; - z lanterny
    ;; - z jakichs natywnych interfejsow javowych
    ;; - z API androida

    (parameterize ((ctrl-pressed? (key:ctrl-down?))
		   (alt-pressed? (key:alt-down?))
		   (shift-pressed? (key:shift-down?)))
      
      (match type
	(,KeyType:Character
	 (invoke (the-top-panel) 'key-typed!
		 (invoke (key:getCharacter) 'charValue)))
	(,KeyType:EOF
	 (io:stopScreen)
	 (exit))

	(,KeyType:MouseEvent
	 (let* ((action ::MouseAction
			(as MouseAction key))
		(position ::TerminalPosition
			  (action:getPosition))
		(left (position:getColumn))
		(top (position:getRow)))
	   (cond ((action:isMouseMove)
		  (values))
		 ((action:isMouseDown)
		  
		  (match (action:getButton)
		    (,MouseButton:Left
		     (values))
		    (,MouseButton:Right
		     (values))
		    (_
		     (values))))
		 ((action:isMouseDrag)
		  (values))
		 ((action:isMouseUp)
		  (values)))))
	
	(_
	 (invoke (the-top-panel) 'key-pressed!
		 type))))
    
    (synchronized screen-up-to-date?
		  (set! (screen-up-to-date?) #f)
		  (invoke screen-up-to-date? 'notify))

    (edit io)
    ))

(set! (on-key-press KeyType:ArrowLeft)
  (lambda ()
    (move-cursor-left! selection: (if (shift-pressed?)
				      Selection:resize
				      Selection:discard))))

(set! (on-key-press KeyType:ArrowRight)
  (lambda ()
    (move-cursor-right! selection: (if (shift-pressed?)
				       Selection:resize
				       Selection:discard))))

(set! (on-key-press KeyType:ArrowUp)
      move-cursor-up!)

(set! (on-key-press KeyType:ArrowDown)
      move-cursor-down!)

(set! (on-key-type #\x) exit)

(define-object (TerminalPainter screen::LanternaScreen)::Painter
  
  (define io::LanternaScreen #!null)

  (define text-color ::Color Color:ANSI:DEFAULT)
  (define background-color ::Color Color:ANSI:DEFAULT)
  
  (define (put! c::char row::real col::real)::void
    (let ((x (+ col shift-left))
          (y (+ row shift-top))
	  (left (max 0 clip-left))
	  (top (max 0 clip-top)))
      (when (and (is left <= x < (+ left clip-width))
                 (is top <= y < (+ top clip-height)))
	(io:setCharacter x y (letter c color: text-color
				     background: background-color)))))

  (define (mark-cursor! +left::real +top::real)::void
    (invoke-special CharPainter (this)
		    'mark-cursor! +left +top)
    (io:setCursorPosition
     (TerminalPosition marked-cursor-position:left
		       marked-cursor-position:top)))
  
  (define (enter-selection-drawing-mode!)::void
    (invoke-special CharPainter (this)
		    'enter-selection-drawing-mode!)
    (set! text-color Color:ANSI:BLACK)
    (set! background-color Color:ANSI:YELLOW))

  (define (exit-selection-drawing-mode!)::void
    (invoke-special CharPainter (this)
		    'exit-selection-drawing-mode!)
    (set! text-color Color:ANSI:DEFAULT)
    (set! background-color Color:ANSI:DEFAULT))
  
  (define (get row::real col::real)::char
    (let ((letter (io:getBackCharacter col row)))
      (letter:getCharacter)))

  (define (clear!)::void
    (io:clear))

  (define (current-width)::real
    (let ((size (io:getTerminalSize)))
      (size:getColumns)))

  (define (current-height)::real
    (let ((size (io:getTerminalSize)))
      (size:getRows)))
  
  (CharPainter)
  (set! io screen))

(define (run
	 #!optional
	 (io :: LanternaScreen (make-terminal-screen)))
  :: void
  (parameterize ((the-painter (TerminalPainter io)))
    (io:startScreen)
    (let* ((editing (future (edit io)))
	   (rendering (future (render io))))
      ;; we want the rendering thread to have a lower
      ;; priority than the editing thread
      (invoke rendering 'setPriority Thread:MIN_PRIORITY)
      (force editing))))

(run)
