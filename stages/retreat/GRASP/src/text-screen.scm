(import (for))
(import (screen))
(import (conversions))

(define-simple-class TextScreen (Screen)
  (shift-left :: real init-value: 0)
  (shift-top :: real init-value: 0)
  (width :: int)
  (height :: int)
  (data :: char[])

  ((put! c::char row::real col::real)::void
   (let ((x (+ col shift-left))
         (y (+ row shift-top)))
     ;; mozna zrobic tak zeby rozszerzac bufor
     ;; kiedy wychodzimy poza zakres
     (when (and (< x width) (>= x 0)
                (< y height) (>= y 0))
       (set! (data (+ (* width y) x)) c))))

  ((*init* w::int h::int)
   (set! width w)
   (set! height h)
   (set! data (char[] length: (* w h)))
   (clear!))

  ((paren-width)::real 2)

  ((clear!)::void
   (for line from 0 below height
        (for row from 0 below width
             (set! (data (+ (* line width) row))
                   #\space)))
   (set! shift-left 0)
   (set! shift-top 0))

  ((toString)::String
   (with-output-to-string
     (lambda ()
       (for line from 0 below height
            (for row from 0 below width
                 (write-char (data (+ (* line width) row))))
            (write-char #\newline)))))

  ((translate! x::real y::real)::void
   (set! shift-left (+ shift-left x))
   (set! shift-top (+ shift-top y)))

  ((draw-finger! left::real top::real index::byte)::Extent
   (cond ((= index 0)
          (put! #\@ top left)
          (Extent width: 1  height: 1))
         (else
          (put! #\( top (- left 1))
          (put! (digit->char index) top left)
          (put! #\) top (+ left 1))
          (Extent width: 3 height: 1))))
         
  ((open-paren! height::real left::real top::real)::void
   (put! #\/ top left)
   (for i from 1 to (- height 2)
        (put! #\| (+ i top) left))
   (put! #\\ (+ top (- height 1)) left))

  ((close-paren! height::real left::real top::real)::void
   (put! #\\ top (+ left 1))
   (for i from 1 to (- height 2)
        (put! #\| (+ i top) (+ left 1)))
   (put! #\/ (+ top (- height 1)) (+ left 1)))

  ((draw-string! s::string left::real top::real)::Extent
   (put! #\" top left) 
   (let ((row top)
         (col (+ left 1))
         (width 1))
     (for c in s
          (cond ((eq? c #\newline)
                 (set! row (+ row 1))
                 (set! col (+ left 1)))
                (else
                 (put! c row col)
                 (set! col (+ col 1))
                 (set! width (max width (- col left))))))
     (put! #\" row (+ left width)) ;")
     (Extent width: (+ width 2)
             height: (- (+ row 1) top))))

  ((draw-text! text::string left::real top::real)::real
   (for i from 0 below (string-length text)
        (put! (string-ref text i) top (+ left i)))
   (string-length text))

  ((draw-atom! text::string)::Extent
   (Extent width: (draw-text! text 0 1)
           height: 3))  

  )
