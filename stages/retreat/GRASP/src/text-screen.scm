(import (for))
(import (primitive))
(import (conversions))
(import (infix))
(import (extent))
(import (screen))

(define-simple-class TextScreen (Screen)
  (shift-left :: real init-value: 0)
  (shift-top :: real init-value: 0)
  (width :: int init-value: 0)
  (height :: int init-value: 0)
  (data :: char[])
  (left :: real init-value: 0)
  (top :: real init-value: 0)
   
  ((get row::real col::real)::char
   (let ((x (+ col shift-left))
         (y (+ row shift-top)))
     (if (and (is 0 <= x < width)
              (is 0 <= y < height))
         (data (+ (* width y) x))
         #\space)))
  
  ((put! c::char row::real col::real)::void
   (let ((x (+ col shift-left))
         (y (+ row shift-top)))
     (when (and (is x >= 0)
                (is y >= 0))
       (when (or (is x >= width)
                 (is y >= height))
         (let* ((new-width (if (is x >= width)
                               (+ x 1)
                               width))
                (new-height (if (is y >= height)
                                (+ y 1)
                                height))
                (new-data (char[] length: (* new-width new-height))))
             (for line from 0 below new-height
                  (for column from 0 below new-width
                       (set! (new-data (+ (* new-width line)
                                          column))
                             (if (and (is column < width)
                                      (is line < height))
                                 (data (+ (* width line)
                                          column))
                                 #\space))))
             (set! width new-width)
             (set! height new-height)
             (set! data new-data)))
       (set! (data (+ (* width y) x)) c))))

  ((paren-width)::real 2)

  ((min-line-height)::real 3)
  
  ((vertical-bar-width)::real 1)

  ((clear!)::void
   (for line from 0 below height
        (for column from 0 below width
             (set! (data (+ (* line width) column))
                   #\space)))
   (set! shift-left 0)
   (set! shift-top 0))

  ((toString)::String
   (with-output-to-string
     (lambda ()
       (write-char #\newline)
       (for line from 0 below height
            (for column from 0 below width
                 (write-char (data (+ (* line width) column))))
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

  ((draw-horizontal-bar! width::real)::void
   (for i from 0 below width
        (when (eq? (get -1 i) #\space)
          (put! #\_ -1 i))))
   
  ((draw-vertical-bar! height::real)::void
   (put! #\: 0 0)
   (for i from 1 below (- height 1)
        (put! #\| i 0))
   (put! #\: (- height 1) 0))
  
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
     (put! #\" row (+ left width))
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
