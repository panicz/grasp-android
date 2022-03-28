(import (define-interface))
(import (define-object))

(define-alias CharSequence java.lang.CharSequence)

(define-interface Screen ()
  (paren-width)::real
  (min-line-height)::real

  (vertical-bar-width)::real
  (horizontal-bar-height)::real
  
  (clear!)::void
  (translate! x::real y::real)::void
  (draw-string! s::CharSequence l::real t::real)::void
  (draw-text! s::CharSequence left::real top::real)::void
  (draw-atom! text::CharSequence)::void
  
  (atom-width text::CharSequence)::real

  (draw-horizontal-bar! width::real)::void
  (draw-vertical-bar! height::real)::void
  (open-paren! height::real l::real t::real)::void
  (close-paren! height::real l::real t::real)::void

  (remember-offset! +left::real +top::real)::void
  (remembered-left)::real
  (remembered-top)::real
  
  ;;(end-line! line-height::real)::void
  ;;(cursor-at left::real top::real)::Cursor
  )

(define-object (NullScreen)::Screen
  (define (paren-width)::real 0)

  (define (min-line-height)::real 0)
  
  (define (vertical-bar-width)::real 0)

  (define (horizontal-bar-height)::real 0)
  
  (define (clear!)::void
    (values))
  
  (define (translate! x::real y::real)::void
    (values))
  
  (define (draw-string! s::CharSequence
			left::real
			top::real)
    ::void
    (values))
  
  (define (draw-text! s::CharSequence
		      left::real
		      top::real)
    ::real
    (length s))
  
  (define (draw-atom! text::CharSequence)::void
    (values))

  (define (atom-width text::CharSequence)::real
    0)
  
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

  (define (remember-offset! +left::real
			    +top::real)
    ::void
    (values))
  
  (define (remembered-left)::real
    0)
  
  (define (remembered-top)::real
    0)
  
  )

(define-constant current-screen::parameter[Screen]
  (make-parameter (NullScreen)))
