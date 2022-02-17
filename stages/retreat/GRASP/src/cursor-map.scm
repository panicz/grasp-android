(import (define-interface))
(import (define-type))
(import (define-object))
(import (screen))
;;(import (recycling))
(import (for))
(import (extent))
(import (infix))
;;(import (string-building))

;; OK, to wyglada na to, ze mamy wciaz jeszcze sporo watpliwosci.
;; Wiec takie male "TODO"/dekompozycja:
;; 1. budowanie struktury danych
;; 2. wyodrebnianie z tej struktury kursorow oraz pozycji
;; 3. recykling/trzymanie zasobow w ryzach


;; i teraz rzecz wyglada tak, ze draw! zwraca nam Extent.
;; ale chcielibysmy tez stworzyc mozliwosc tworzenia nowej
;; 'mapy kursorow'
;; pytanie: czy tworzenie mapy kursorow tylko przez draw-sequence!
;; ma szanse zdac egzamin?
;;
;; trudnosc polega na tym, ze nie jest jasne, w jaki sposob
;; funkcja mialaby zwracac nowy obiekt typu CursorMap.
;; Stad pomysl jest taki, zeby funkcja draw! zwracala albo Extent,
;; albo CursorMap. Albo zeby klasa Demarcator dziedziczyla po Extent,
;; ale dodatkowo implementowala interfejs CursorMap.
;;
;; I teraz: funkcja draw-sequence! sprawdzalaby, czy
;; zwrocony obiekt to jest 'CursorMap', i jesli tak,
;; to dodawalaby go do biezacego dziadostwa

(define-alias ArrayList java.util.ArrayList)

(define-type (Area left: real
		   top: real
		   width: real
		   height: real
		   cursor: Cursor
		   nested: Area))

(define-type (Section width: real
		      height: real
		      cursor: Cursor
		      inner: CursorMap))

(define-type (Line width: real := 0
		   height: real := (invoke (current-screen) 'min-line-height)
		   sections: ArrayList[Section] := (ArrayList)))

(define-interface CursorMap ()
  (area-under left::real top::real)::Area
  (clear!)::void
  (add-section! extent::Extent cursor::Cursor)::void
  (end-line!)::void)

(define-object (LinearCursorMap)::CursorMap
  (define cursor ::Cursor)
  (define lines ::ArrayList[Line] (ArrayList))
  
  (define (area-under left::real top::real)::Area
    (call/cc
     (lambda (return)
       (let ((ceiling 0))
	 (for line :: Line in lines
	      (when (is top < (+ ceiling line:height))
		(let ((side 0))
		  (for section :: Section in line:sections
		       (when (is left < (+ side section:width))
			 (return
			  (Area left: side
				top: ceiling
				width: section:width
				height: section:height
				cursor: section:cursor
				nested: (if section:inner
					    (section:inner:area-under
					     (- left side)
					     (- top ceiling))
					    #!null))))
		       (set! side (+ side section:width)))))
	      (set! ceiling (+ ceiling line:height))))
       #!null)))

  (define (clear!)::void
    (lines:clear)
    (lines:add (Line))
    (set! width 0)
    (set! height 0))

  (define (add-section! extent::Extent cursor::Cursor)::void
    (let ((line (lines (- (lines:size) 1))))
      (set! line:height (max line:height extent:height))
      (set! line:width (+ line:width extent:width))
      (set! width (max width line:width))
      (line:sections:add (Section width: extent:width
				  height: extent:height
				  cursor: cursor
				  inner: (if (instance? extent CursorMap)
					     (as CursorMap extent)
					     #!null)))))

  (define (end-line!)::void
    (lines:add (Line)))

  (define (toString)::String
    "LinearCursorMap")
  
  (Extent)
  (lines:add (Line)))

