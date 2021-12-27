(import (sweetex))

(use-sweetex)

;; No to takie mamy miec rzeczy dwie:
;; - wartoscia wyrazenia moze byc wizualny obiekt
;; - samo wyrazenie moze byc wizualizowane w jakis
;;   szczegolny sposob

;; Na razie to mamy

(define-alias List/int gnu.lists.LList)

(define-alias List/Pad gnu.lists.LList)

(define-interface Canvas ()
  (translate x::float y::float)::void
  (quickReject left::float top::float
	       right::float bottom::float)::boolean
  )

(define-interface Navigable (Pad)
  (at index::int)::Node
  (size)::int
  (first-cursor)::List/int
  ;; if source was a previously selected cursor
  ;; (presumably - to a following expression)
  ;; return a cursor that is considered
  ;; 'first' within the current Navigable.
  ;; If the Navigable structure is flat,
  ;; the expected implementation is '(0).
  ;; Otherwise, if it is nested, we expect
  ;; it to be
  ;; `(0 . ,(if (= (size) 0)
  ;;          '()
  ;;          ((at 0):first-cursor))
  (last-cursor)::List/int
  ;; like 'first-cursor', but instead of the
  ;; first subexpression, we consider
  ;; the last one, so the flat implementation
  ;; is expected to be
  ;; `(if (= (size) 0) '(0) `(,(- (size) 1)))
  ;; Otherwise, if it is nested, we expect
  ;; it to be
  ;; (if (= (size) 0)
  ;;     '(0)
  ;;     `(,(- (size) 1))
  ;;       . ,((at 0):first-cursor)))
  )

(define (! n)
  (if (= n 0)
      1
      (* n (! (- n 1)))))

(define (cursor-next cursor::List/int content::Navigable)
  (match cursor
    (`(,tip)
     (if (is (+ tip 1) < (content:size))
	 `(,(+ tip 1))
	 cursor))

    (`(,this . ,next)
     (let* ((next* (cursor-next next (content:at this))))
       (if (isnt next* equal? next)
	   `(,this . ,next*)
	   (if (is (+ this 1) < (content:size))
	       `(,(+ this 1) . ,((content:at (+ this 1)):first-cursor))
	       cursor))))
    ('()
     '())))

(define (cursor-back cursor::List/int content::Navigable)
  (match cursor
    (`(0)
     cursor)

    (`(,n)
     `(,(- n 1)))

    (`(,this . ,next)
     (let* ((back (cursor-back next (content:at this))))
       (if (isnt back equal? next)
	   `(,this . ,back)
	   (if (is this > 0)
	       `(,(- this 1) . ,((content:at (- this 1)):last-cursor))
	       cursor))))
    ('()
     '())))

;; No dobra, ale o co tak dokladnie nam chodzi?
;; rozwazamy kwestie poruszania kursorem po spacjach
;; pomiedzy wyrazeniami.
;; Chcielibysmy uzyc reprezentacji ze slabymi hasz-mapami.
;; W taki sposob ostatni indeks kursora bedzie wyrazal jego pozycje
;; wsrod 'bialych znakow'

(define line-height (make-parameter 72))

(define-type (Document contents: List/Pad =: '())
  implementing Pad
  with
  ((render canvas::Canvas cursor)::void
   (let ((total-height 0)
	 (current 0)
	 (selected (and (pair? cursor)
			(head cursor))))
     (when (and selected (= selected current))
       ...)
     (for expression in contents
	  (let ((w (width expression))
		(h (height expression)))
	    (set! current (+ current 1))
	    (unless (canvas:quickReject 0 0 w h #;Canvas.EdgeType.BW)
	      (expression:render canvas (and selected
					     (= selected current)
					     (tail cursor))))
	    (canvas:translate 0 h)
	    (set! total-height (+ total-height h))
	    (set! current (+ current 1))
	    (when (and selected (= selected current))
	      ...)))
     (canvas:translate 0 (- total-height))))
  ((width)::float
   (fold-left (lambda (max-width next-element)
		(max max-width (width next-element)))
	      0 contents))
  ((height)::float
   (fold-left (lambda (total-height next-element)
		(+ total-height (height next-element)))
	      0 contents)))

(define-interface Renderable ()
  (render-on canvas::Canvas)::void)

(define-type (Editor active-document: document
		     cursor: List/int
		     transform: Transform2D)
  implementing Renderable
  with
  ((render-on canvas::Canvas)
   (transform:transform canvas)
   (document:render canvas cursor 0)
   (transform:untransform canvas)))


;; TODO lista:
;; 1. model edycji dokumentu + testy/przyklady
;; 2. komponowanie wygladu i interakcji
;; 3

(define (render-pair p::Pair canvas::Canvas cursor)
  (cond ((null? (head p))
	 (parenthesized
	  (Space width:
		 (space-width (null-head-space p))
		 height:
		 (space-height (null-head-space p)))))
	(else
	 (show (head p))))
  (display (post-head-space p))
  (cond ((dotted? p)
	 (write-char #\.)
	 (display (pre-tail-space p))
	 (cond ((null? (tail p))
		(write-char #\()
		(display (null-tail-space p))
		(write-char #\)))
	       (else
		(show (tail p))))
	 (display (post-tail-space p))
	 (write-char #\)))
	((null? (tail p))
	 (write-char #\)))
	(else
	 ;;(assert (pair? (tail p)))
	 (show-pair (tail p)))))

(define (show p)
  (cond
   ((pair? p)
    (Beside
    (write-char #\()
    (display (pre-head-space p))
    (show-pair p))
   (else
    (write p)))))

;; kolejne podejscie do renderowania:

(define (EmptyList space)::Pad
  (Beside
   (LeftParen (space-height space))
   (Space width: (space-width space)
	  height: (space-height space))
   (RightParen (space-height space))))

(define (Head p::Pair)::Pad
  (if (null? (head p))
      (EmptyList (null-head-space p))
      (Layout (head p))))

(define (Tail p::Pair)::Pad
  (cond ((dotted? p)
	 (DottedTail p))
	((pair? (tail p))
	 (show-pair (tail p)))))

(define (DottedTail p::Pair)::Pad
  (write-char #\.)
  (display (post-dot-space p))
  (if (null? (tail p))
      (show-empty-list (null-tail-space p))
      (show (tail p)))
  (display (post-tail-space p)))

(define (Sequence p::Pair)::Pad
  (if (breaks-line? (post-head-space p))
      (Below (Head p) (Tail p))
      (Beside (Head p) (Tail p))))


(define (Layout p)::Pad
  (cond
   ((pair? p)
    (write-char #\()
    (display (post-open-paren-space p))
    (show-pair p)
    (write-char #\)))
   (else
    (write p))))

;; najnowszy plan jest taki, ze chcemy miec dwa renderery:
;; jeden do trybu tekstowego (curses) i drugi dla androida,


"
/         /             \     \
| define  | factorial n |     |
|         \             /     |
| /     /        \          \ |
| | if  | <= n 0 |          | |
| |     \        /          | |
| |        n                | |
| |    1                    | |
| |                         | |
| |  /        /   /       \ | |
| |  |  *  n  | ! | - n 1 | | |
\ \  \        \   \       / / /
"

;; Chcemy zdefiniowac jeden interfejs i zrobic dla niego
;; dwie implementacje

;; Pierwsza kwestia:

;; rysowac bedziemy do tablicy,
;; natomiast te tablice bedziemy pozniej
;; konwertowac do stringa

