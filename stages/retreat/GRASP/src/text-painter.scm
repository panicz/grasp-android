(import (for))
(import (fundamental))
(import (primitive))
(import (conversions))
(import (infix))
(import (extent))
(import (painter))
(import (define-object))
(import (indexable))

(define-object (CharPainter)::Painter
  (define shift-left ::real 0)
  (define shift-top ::real 0)

  (define clip-left ::real 0)
  (define clip-top ::real 0)
  (define clip-width ::real +inf.0)
  (define clip-height ::real +inf.0)

  (define (clip! left::real  top::real
		 width::real height::real)
    ::void
    (set! clip-left left)
    (set! clip-top top)
    (set! clip-width width)
    (set! clip-height height))
		 
  (define (current-clip-width)::real
    clip-width)
  
  (define (current-clip-height)::real
    clip-height)
  
  (define (current-clip-left)::real
    clip-left)
  
  (define (current-clip-top)::real
    clip-top)
  
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

  (define (space-width)::real 1)
  
  (define (paren-width)::real 2)

  (define (min-line-height)::real 3)
  
  (define (vertical-bar-width)::real 1)

  (define (horizontal-bar-height)::real 1)
  
  (define (translate! x::real y::real)::void
    (set! shift-left (+ shift-left x))
    (set! shift-top (+ shift-top y)))
  
  (define (current-translation-left)::real
    shift-left)
  
  (define (current-translation-top)::real
    shift-top)

  (define (draw-horizontal-line! top::real)::void
    (for i from (max 0 (current-clip-left))
      below (min (current-width) (clip-width))
      (put! #\─ top i)))
  
  (define (draw-vertical-line! left::real)::void
    (for i from (max 0 (current-clip-top))
      below (min (current-height) (clip-height))
      (put! #\│ i left)))

  (define (horizontal-line-height)::real
    1)
  
  (define (vertical-line-width)::real
    1)
  
  (define (draw-horizontal-bar! width::real)::void
    (for i from 0 below width
         (when (eq? (get -1 i) #\space)
           (put! #\_ -1 i))))
   
  (define (draw-vertical-bar! height::real)::void
    (put! #\╷ 0 0)
    (for i from 1 below (- height 1)
         (put! #\│ i 0))
    (put! #\╵ (- height 1) 0))

  (define (open-paren! height::real)
    ::void
    (put! #\╭ 0 0)
    (for i from 1 to (- height 2)
         (put! #\│ i 0))
    (put! #\╰ (- height 1) 0))
  
  (define (close-paren! height::real)
    ::void
    (put! #\╮ 0 1)
    (for i from 1 to (- height 2)
         (put! #\│ i 1))
    (put!  #\╯ (- height 1) 1))

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
    (let ((extent ::Extent (string-extent s)))
      (put! #\❝ 0 0)
      (with-translation (2 1)
	  (draw-string! s index))
      (put! #\❞ (+ extent:height 1) (+ extent:width 3))))

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
	       (put! c row col)
	       (set! col (+ col 1))))
	(set! n (+ n 1)))))

  (define (string-character-index-under x::real y::real
					text::CharSequence)
    ::int
    (let ((end (text:length)))
      (let next ((row 0)
		 (col 0)
		 (n ::int 0))
	(if (or (and (is x = col) (is y <= row))
		(is n >= end))
	    n
	    (let ((c (text n)))
	      (if (eq? c #\newline)
		  (if (is y <= row)
		      n
		      (next (+ row 1) 0 (+ n 1)))
		  (next row (+ col 1) (+ n 1))))))))
  
  (define (quoted-text-extent text::CharSequence)::Extent
    (let ((inner ::Extent (string-extent text)))
      (Extent width: (+ inner:width 4)
	      height: (+ (max inner:height 1) 2))))

  (define (quoted-text-character-index-under x::real y::real
					     text::CharSequence)
    ::int
   (string-character-index-under (- x 2) (- y 1) text))
  
  (define (draw-atom! text::CharSequence index::Index)::void
    (with-translation (0 1)
	(draw-string! text index)))

  (define (atom-extent text::CharSequence)::Extent
    (let ((inner ::Extent (string-extent text)))
      (Extent width: inner:width
	      height: (max (min-line-height) inner:height))))

  (define (atom-character-index-under x::real y::real
				      text::CharSequence)
    ::int
    (string-character-index-under x (- y 1) text))
  
  (define (get row::real col::real)::char #!abstract)

  (define (put! c::char row::real col::real)::void #!abstract)

  (define (clear!)::void #!abstract)

  (define (current-width)::real #!abstract)

  (define (current-height)::real #!abstract)
  
  )
  
  
(define-object (TextPainter)::Painter
  (define width ::int 0)
  (define height ::int 0)
  (define data ::char[])

  (define (get row::real col::real)::char
    (let ((x (+ col shift-left))
          (y (+ row shift-top)))
      (if (and (is 0 <= x < width)
               (is 0 <= y < height))
          (data (+ (* width y) x))
          #\space)))
  
  (define (put! c::char row::real col::real)::void
    (let ((x (+ col shift-left))
          (y (+ row shift-top))
	  (left (max 0 clip-left))
	  (top (max 0 clip-top)))
      (when (and (is left <= x < (+ left clip-width))
                 (is top <= y < (+ top clip-height)))
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

  (define (current-width)::real width)

  (define (current-height)::real height)
  
  (CharPainter))

