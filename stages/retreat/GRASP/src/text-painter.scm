(import (srfi :11))
(import (for))
(import (fundamental))
(import (primitive))
(import (conversions))
(import (infix))
(import (extent))
(import (painter))
(import (define-object))
(import (indexable))
(import (cursor))
(import (match))
(import (space))
(import (functions))

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

  (define marked-cursor-position ::Position
    (Position left: 0
	      top: 0))
  
  (define (mark-cursor! +left::real +top::real)::void
    (set! marked-cursor-position:left (+ shift-left +left))
    (set! marked-cursor-position:top (+ shift-top +top))
    )

  (define (cursor-position)::Position
    marked-cursor-position)
  
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

  (define (draw-box! width::real height::real context::Cursor)::void
    (when (and (pair? (the-cursor))
	       (equal? context (tail (the-cursor))))
      (match (head (the-cursor))
	(#\[ (mark-cursor! 0 1))
	(#\] (mark-cursor! (- width 1) (- height 2)))
	(_ (values))))
    (put! #\╭ 0 0)
    (for i from 1 to (- height 2)
         (put! #\│ i 0))
    (put! #\╰ (- height 1) 0)
    (put! #\╮ 0 (- width 1))
    (for i from 1 to (- height 2)
         (put! #\│ i (- width 1)))
    (put!  #\╯ (- height 1) (- width 1)))
  
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
  
  (define (draw-quoted-text! s::CharSequence context::Cursor)
    ::void
    (let ((extent ::Extent (string-extent s)))
      (put! #\❝ 0 0)
      (put! #\• 0 (+ extent:width 3))
      (for i from 1 below (+ extent:width 3)
	   (put! #\┈ (+ extent:height 1) i)
	   (put! #\┈ 0 i))
      (for i from 1 to extent:height
	   (put! #\┊ i 0)
	   (put! #\┊ i (+ extent:width 3)))
      (put! #\• (+ extent:height 1) 0)
      (put! #\❞ (+ extent:height 1) (+ extent:width 3))
      (with-translation (2 1)
	  (draw-string! s context))
      (put! #\❞ (+ extent:height 1) (+ extent:width 3))))

  (define (draw-string! text::CharSequence context::Cursor)::void
    (let-values (((selection-start selection-end) (the-selection)))
      (let ((focused? (and (pair? (the-cursor))
			   (equal? context (cursor-tail))))
	    (alters-selection-drawing-mode?
	     (or (and (pair? selection-start)
		      (equal? (tail selection-start) context))
		 (and (pair? selection-end)
		      (equal? (tail selection-end) context))))
	    (row 0)
	    (col 0)
	    (n 0))
	
	(define (handle-cursor-and-selection!)
	  (when alters-selection-drawing-mode?
	    (when (eqv? n (head selection-start))
	      (enter-selection-drawing-mode!))
	    (when (eqv? n (head selection-end))
	      (exit-selection-drawing-mode!)))
	  (when (and focused? (eqv? n (cursor-head)))
	    (mark-cursor! col row)))
	
	(for c in text
	  (handle-cursor-and-selection!)
          (cond ((eq? c #\newline)
		 (set! row (+ row 1))
		 (set! col 0))
		(else
		 (put! c row col)
		 (set! col (+ col 1))))
	  
	  (set! n (+ n 1)))
	(handle-cursor-and-selection!))))

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
  
  (define (draw-atom! text::CharSequence context::Cursor)::void
    (with-translation (0 1)
	(draw-string! text context)))

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

  (define selection-drawing-mode? ::boolean #f)
  
  (define (enter-selection-drawing-mode!)::void
    (set! selection-drawing-mode? #t))

  (define (exit-selection-drawing-mode!)::void
    (set! selection-drawing-mode? #f))
  
  (define (in-selection-drawing-mode?)::boolean
    selection-drawing-mode?)
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
	(set! (data (+ (* width y) x)) c)
	(when (and selection-drawing-mode?
		   (is (+ y 1) < height))
	  (set! (data (+ (* width (+ y 1)) x)) #\~)))))

  (define (clear!)::void
   (for line from 0 below height
        (for column from 0 below width
             (set! (data (+ (* line width) column))
                   #\space)))
   (set! shift-left 0)
   (set! shift-top 0))

  (define (mark-cursor! +left::real +top::real)::void
    (invoke-special CharPainter (this) 'mark-cursor! +left +top)
    (match (the-expression)
      (,@Space?
       (put! #\| (+ +top 1) +left))
      (,@Atom?
       (put! #\^ (+ +top 1) +left))
      (_
       (values))))
  
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
