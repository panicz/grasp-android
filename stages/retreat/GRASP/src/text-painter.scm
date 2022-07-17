(import (for))
(import (primitive))
(import (conversions))
(import (infix))
(import (extent))
(import (painter))
(import (define-object))
(import (indexable))

(define-object (TextPainter)::Painter
  (define shift-left ::real 0)
  (define shift-top ::real 0)
  (define width ::int 0)
  (define height ::int 0)
  (define data ::char[])
  
  (define left ::real 0)
  (define top ::real 0)

  (define (remember-offset! +left::real +top::real)
    ::void
    (set! left (+ shift-left +left))
    (set! top (+ shift-top +top)))
  
  (define (remembered-left)::real
    left)
  
  (define (remembered-top)::real
    top)
  
  (define (get row::real col::real)::char
    (let ((x (+ col shift-left))
          (y (+ row shift-top)))
      (if (and (is 0 <= x < width)
               (is 0 <= y < height))
          (data (+ (* width y) x))
          #\space)))
  
  (define (put! c::char row::real col::real)::void
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
                 (new-data (char[] length: (* new-width
					      new-height))))
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

  (define (paren-width)::real 2)

  (define (min-line-height)::real 3)
  
  (define (vertical-bar-width)::real 1)

  (define (horizontal-bar-height)::real 1)
  
  (define (clear!)::void
   (for line from 0 below height
        (for column from 0 below width
             (set! (data (+ (* line width) column))
                   #\space)))
   (set! shift-left 0)
   (set! shift-top 0))

  (define (toString)::String
    (with-output-to-string
      (lambda ()
	(write-char #\newline)
	(for line from 0 below height
             (for column from 0 below width
                  (write-char (data (+ (* line width)
				       column))))
             (write-char #\newline)))))

  (define (translate! x::real y::real)::void
    (set! shift-left (+ shift-left x))
    (set! shift-top (+ shift-top y)))

  (define (draw-horizontal-bar! width::real)::void
    (for i from 0 below width
         (when (eq? (get -1 i) #\space)
           (put! #\_ -1 i))))
   
  (define (draw-vertical-bar! height::real)::void
    (put! #\╷ 0 0)
    (for i from 1 below (- height 1)
         (put! #\│ i 0))
    (put! #\╵ (- height 1) 0))

  (define (open-paren! height::real left::real top::real)
    ::void
    (put! #\╭ top left)
    (for i from 1 to (- height 2)
         (put! #\│ (+ i top) left))
    (put! #\╰ (+ top (- height 1)) left))
  
  (define (close-paren! height::real left::real top::real)
    ::void
    (put! #\╮ top (+ left 1))
    (for i from 1 to (- height 2)
         (put! #\│ (+ i top) (+ left 1)))
    (put!  #\╯ (+ top (- height 1)) (+ left 1)))

  (define (draw-rounded-rectangle! width::real
				   height::real)
    ::void
    (put! #\╭ 0 0)
    (for i from 1 to (- height 2)
         (put! #\│ i 0))
    (put! #\╰ (- height 1) 0)

    (for i from 1 to (- width 2)
         (put! #\─ 0 i)
	 (put! #\─ (- height 1) i))
    
    (put! #\╮ 0 (- width 1))
    (for i from 1 to (- height 2)
         (put! #\│ i (- width 1)))
    (put! #\╯ (- height 1) (- width 1))
    
    (values))
  
  (define (draw-quoted-text! s::CharSequence index::Index)
    ::void
    (let ((extent (string-extent s)))
      (put! #\❝ 0 0)
      (with-translation (2 1)
	  (draw-string! s index))
      (put! #\❞ (+ extent:height 1) (+ extent:width 2))))

  (define (draw-string! text::CharSequence index::Index)::void
    (let ((row 0)
	  (col 0)
	  (n 0))
      (for c in text
	(when (eqv? n index)
	  (remember-offset! col (+ row 1)))
        (cond ((eq? c #\newline)
	       (set! row (+ row 1))
	       (set! col 0))
	      (else
	       (put! c row col)	       (set! col (+ col 1))))
	(set! n (+ n 1)))))
  
  (define (quoted-text-extent text::CharSequence)::Extent
    (let ((inner ::Extent (string-extent text)))
      (Extent width: (+ inner:width 4)
	      height: (+ (max inner:height 1) 2))))
  
  (define (draw-atom! text::CharSequence index::Index)::void
    (with-translation (0 1)
	(draw-string! text index)))

  (define (atom-extent text::CharSequence)::Extent
    (string-extent text))
  
  )
