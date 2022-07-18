(import (define-interface))
(import (define-object))
(import (define-syntax-rule))
(import (define-parameter))
(import (default-value))
(import (extent))
(import (indexable))

(define-alias CharSequence java.lang.CharSequence)

(define-interface Translatable ()
  (translate! x::real y::real)::void
  (current-translation-left)::real
  (current-translation-top)::real
  )

(define-interface Rotatable ()
  (rotate! angle::real)::void
  (current-rotation-angle)::real
  )

(define-interface Clippable ()
  (clip! left::real  top::real
	 width::real height::real)::void	 
  (current-clip-width)::real
  (current-clip-height)::real
  (current-clip-left)::real
  (current-clip-top)::real
  )  

(define-interface Scalable ()
  (scale! factor::real)::void
  (current-scale)::real
  )

(define-interface Splittable ()
  (draw-horizontal-line! top::real)::void
  (draw-vertical-line! left::real)::void
  (horizontal-line-height)::real
  (vertical-line-width)::real
  )

(define-interface Painter (Splittable Clippable Translatable)
  (paren-width)::real
  (min-line-height)::real

  (vertical-bar-width)::real
  (horizontal-bar-height)::real
  
  (clear!)::void
    
  (draw-quoted-text! s::CharSequence index::Index)::void
  (draw-string! s::CharSequence index::Index)::void
  (quoted-text-extent text::CharSequence)::Extent
  
  (draw-atom! text::CharSequence index::Index)::void
  
  (atom-extent text::CharSequence)::Extent

  (draw-horizontal-bar! width::real)::void
  (draw-vertical-bar! height::real)::void
  (open-paren! height::real l::real t::real)::void
  (close-paren! height::real l::real t::real)::void

  (draw-rounded-rectangle! width::real height::real)::void
  
  (remember-offset! +left::real +top::real)::void
  (remembered-left)::real
  (remembered-top)::real
  
  ;;(end-line! line-height::real)::void
  ;;(cursor-at left::real top::real)::Cursor
  )

(define-object (NullPainter)::Painter
  (define (paren-width)::real 0)

  (define (min-line-height)::real 0)
  
  (define (vertical-bar-width)::real 0)

  (define (horizontal-bar-height)::real 0)
  
  (define (clear!)::void
    (values))
  
  (define (translate! x::real y::real)::void
    (values))

  (define (current-translation-left)::real
    0)
  
  (define (current-translation-top)::real
    0)

  (define (clip! left::real  top::real
		 width::real height::real)
    ::void
    (values))
  
  (define (current-clip-width)::real
    0)
  
  (define (current-clip-height)::real
    0)
  
  (define (current-clip-left)::real
    0)
  
  (define (current-clip-top)::real
    0)
  
  (define (draw-horizontal-line! top::real)::void
    (values))
  
  (define (draw-vertical-line! left::real)::void
    (values))

  (define (horizontal-line-height)::real
    0)
  
  (define (vertical-line-width)::real
    0)
  
  (define (draw-quoted-text! s::CharSequence
			     index::Index)
    ::void
    (values))
  
  (define (draw-string! s::CharSequence index::Index)
    ::void
    (values))

  (define (quoted-text-extent text::CharSequence)::Extent
    (Extent width: 0 height: 0))
  
  (define (draw-atom! text::CharSequence index::Index)::void
    (values))

  (define (atom-extent text::CharSequence)::Extent
    (Extent width: 0 height: 0))
  
  (define (draw-horizontal-bar! width::real)::void
   (values))
  
  (define (draw-vertical-bar! height::real)::void
   (values))
  
  (define (open-paren! height::real
		       left::real
		       top::real)
    ::void
    (values))
  
  (define (close-paren! height::real
			left::real
			top::real)
    ::void
   (values))

  (define (draw-rounded-rectangle! width::real
				   height::real)
    ::void
    (values))

  
  (define (remember-offset! +left::real
			    +top::real)
    ::void
    (values))
  
  (define (remembered-left)::real
    0)
  
  (define (remembered-top)::real
    0)
  
  )

(set! (default-value Painter) (NullPainter))

(define-parameter (the-painter) ::Painter)

(define-syntax-rule (with-translation (x y) . actions)
  (let ((painter (the-painter))
	(x! x)
        (y! y))
    (invoke painter 'translate! x! y!)
    (begin . actions)
    (invoke painter 'translate! (- x!) (- y!))))

(define-syntax-rule (with-clip (w h) . actions)
  (let ((painter (the-painter)))
    (let ((x0 (invoke painter 'current-clip-left))
	  (y0 (invoke painter 'current-clip-top))
	  (w0 (invoke painter 'current-clip-width))
	  (h0 (invoke painter 'current-clip-height))
	  (x! (invoke painter 'current-translation-left))
	  (y! (invoke painter 'current-translation-top))
	  (w! w)
	  (h! h))
      (invoke painter 'clip! x! y! w! h!)
      (begin . actions)
      (invoke painter 'clip! x0 y0 w0 h0))))
