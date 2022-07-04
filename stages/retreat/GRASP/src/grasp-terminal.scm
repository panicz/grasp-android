;; (import (define-syntax-rule))
;; (import (define-interface))
;; (import (define-type))
;; (import (define-object))
;; (import (extent))
;; (import (conversions))
;; (import (indexable))
;; (import (space))
;; (import (cursor))
;; (import (tile))
;; (import (primitive))
;; (import (extent))
;; (import (text-screen))
;; (import (combinators))
;; (import (parse))
;; (import (symbol))
;; (import (examples))
;; (import (assert))
(import (infix))
(import (match))
(import (term))
(import (functions))
;; (import (print))
;; (import (screen))
(import (for))
(import (while))
;; (import (document-operations))
;; (import (editor-operations))
;; (import (extension))
;; (import (button))

(define-alias Thread java.lang.Thread)

(define screen-up-to-date? :: parameter[boolean]
  (make-parameter #f))

(define x 0)
(define y 0)

(define X (TextCharacter #\X))

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
		;; 1981 paper "EMACS: The Extensible, Customizable
		;; Display Editor", section 13 "The Display
		;; Processor")
		(set! (screen-up-to-date?) #t))
  ;; tutaj chcemy sobie wywolac funkcje rysujaca
  ;; aczkolwiek na poczatek wystarczy po prostu rysowac
  ;; pojedynczy znaczek, przesuwany za pomoca klawiszy strzalek,
  ;; albo cos takiego
  (io:clear)
  (io:setCharacter x y X)
  (io:refresh) ;; swap front- and back-buffer
  (render io))
  
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
       (set! x (max 0 (- x 1))))
      (,KeyType:ArrowRight
       (set! x (+ x 1)))
      (,KeyType:ArrowUp
       (set! y (max 0 (- y 1))))
      (,KeyType:ArrowDown
       (set! y (+ y 1)))
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

(define (run #!optional (io :: LanternaScreen (make-terminal-screen)))
  :: void
  (parameterize ()
    (io:startScreen)
    (io:setCursorPosition #!null)
    (let* ((editing (future (edit io)))
	   (rendering (future (render io))))
      ;; we want the rendering thread to have a lower
      ;; priority than the editing thread
      (invoke rendering 'setPriority Thread:MIN_PRIORITY)
      (force editing))))

(run)
