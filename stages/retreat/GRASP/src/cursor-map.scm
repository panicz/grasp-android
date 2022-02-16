(import (define-interface))
(import (define-type))
(import (screen))
(import (recycling))
(import (for))

;; OK, to wyglada na to, ze mamy wciaz jeszcze sporo watpliwosci.
;; Wiec takie male "TODO"/dekompozycja:
;; 1. budowanie struktury danych
;; 2. wyodrebnianie z tej struktury kursorow oraz pozycji
;; 3. recykling/trzymanie zasobow w ryzach


(define-type (Area left: real
		   top: real
		   width: real
		   height: real
		   content: Object))

(define-interface CursorMap ()
  (area-below left::real top::real)::Area
  (clear)::void
  (add-section! width::real height::real cursor::Cursor)::void
  (end-line)::void
  (position cursor::Cursor)::Point)


(define-type (Section width: real
		      cursor: Cursor
		      line))

(define-type (Line height: := (invoke (current-screen)
				      'min-line-height)
		   content: ArrayList[Section] := (ArrayList)))

(define-object (Demarcator)::CursorMap
  (define lines ::ArrayList[Line] (ArrayList (Line)))
  
  (define (clear)::void
    (lines:clear)
    (lines:add (Line)))x

  (define (add-section! width::real height::real cursor::Cursor)::void
    (let ((line ::Line (lines (- (lines:size) 1))))
      (set! line:height (max line:height height))
      (line:content:add (Recycled Section
				  width: width
				  cursor: cursor))))

  (define (end-line!)::void
    (lines:add (Recycled Line
			 height: (invoke (current-screen)
					 'min-line-height))))

  (define (cursor-at left::real top::real)::Cursor
    (call/cc
     (lambda (return)
       (let ((ceiling 0))
	 (for line in lines
	   (when (is top < (+ ceiling line:height))
	     (let ((side 0))
	       (for section in line:sections
		 (when (is right < (+ side section:width))
		   (if (eq? section:inner #!null)
		       (return section:cursor)
		       (return (section:inner:cursor-at
				(- left side)
				(- top ceiling))))))
	       (set! side (+ side section:width)))))
	 (set! ceiling (+ ceiling line:height)))
       (return #!null))))
  
  (define (position cursor::Cursor)::Point
    ...))
