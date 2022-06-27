(import (define-interface))
(import (define-object))
(import (define-syntax-rule))
(import (extent))
(import (indexable))

(define-alias CharSequence java.lang.CharSequence)

(define-interface Screen ()
  (paren-width)::real
  (min-line-height)::real

  (vertical-bar-width)::real
  (horizontal-bar-height)::real
  
  (clear!)::void
  (translate! x::real y::real)::void
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

(define-object (NullScreen)::Screen
  (define (paren-width)::real 0)

  (define (min-line-height)::real 0)
  
  (define (vertical-bar-width)::real 0)

  (define (horizontal-bar-height)::real 0)
  
  (define (clear!)::void
    (values))
  
  (define (translate! x::real y::real)::void
    (values))
  
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

(define-constant the-screen::parameter[Screen]
  (make-parameter (NullScreen)))

(define-syntax-rule (with-translation (x y) . actions)
  (let ((x! x)
        (y! y))
    (invoke (the-screen) 'translate! x! y!)
    (begin . actions)
    (invoke (the-screen) 'translate! (- x!) (- y!))))
