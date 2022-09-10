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

(define-parameter (screen-up-to-date?)::boolean #f)

(define-parameter (the-text-style)::TextStyle
  (TextStyle:noneOf TextDecoration:class))

(define-parameter (the-text-color)::Color
  Color:ANSI:WHITE)

(define-parameter (the-background-color)::Color
  Color:ANSI:BLACK)

(define-cache (letter
	       character
	       color: color::Color := (the-text-color)
	       background: background::Color
	       := (the-background-color)
	       style: style::TextStyle := (the-text-style))
  ::Letter
  (Letter character color background style))

(define (render io :: LanternaScreen)::void
  (synchronized screen-up-to-date?
		(when (screen-up-to-date?)
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
  (let ((size ::TerminalSize (or (io:doResizeIfNecessary)
				 (io:getTerminalSize)))
	(extent ::Extent (the-screen-extent)))
    (set! extent:width (size:getColumns))
    (set! extent:height (size:getRows)))
  (let ((painter (the-painter)))
    (painter:clear!)
    (invoke (the-top-panel) 'draw! ())
     ;; swap front- and back-buffer
    (io:refresh LanternaScreen:RefreshType:DELTA)
    (render io)))
  
(define (edit io :: LanternaScreen)::void
  (let* ((key ::KeyStroke (io:readInput))
	 (type ::KeyType (key:getKeyType)))

    ;; powinnismy sobie wymyslic jakas warstwe abstrakcji,
    ;; ktora obslugiwalaby klawisze:
    ;; - z lanterny
    ;; - z jakichs natywnych interfejsow javowych
    ;; - z API androida

    (match type
      (,KeyType:ArrowLeft
       (values))
      (,KeyType:ArrowRight
       (values))
      (,KeyType:ArrowUp
       (values))
      (,KeyType:ArrowDown
       (values))
      (,KeyType:EOF
       (io:stopScreen)
       (exit))
      (,KeyType:Escape
       (io:stopScreen)
       (exit))
      (_
       (values)))
    
    (synchronized screen-up-to-date?
		  (set! (screen-up-to-date?) #f)
		  (invoke screen-up-to-date? 'notify))

    (edit io)
    ))

(define-object (TerminalPainter screen::LanternaScreen)::Painter

  (define io::LanternaScreen #!null)
  
  (define (put! c::char row::real col::real)::void
    (let ((x (+ col shift-left))
          (y (+ row shift-top))
	  (left (max 0 clip-left))
	  (top (max 0 clip-top)))
      (when (and (is left <= x < (+ left clip-width))
                 (is top <= y < (+ top clip-height)))
	(io:setCharacter x y (letter c)))))

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
