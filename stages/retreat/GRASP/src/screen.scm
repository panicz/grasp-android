(import (define-interface))
(import (define-object))
(import (extent))

(define-interface Screen ()
  (paren-width)::real
  (min-line-height)::real
  (vertical-bar-width)::real

  (clear!)::void
  (translate! x::real y::real)::void
  (draw-string! s::string left::real top::real)::Extent
  (draw-text! s::string left::real top::real)::real
  (draw-atom! text::string)::Extent
  (draw-finger! left::real top::real index::byte)::Extent
  (draw-horizontal-bar! width::real)::void
  (draw-vertical-bar! height::real)::void
  (open-paren! height::real left::real top::real)::void
  (close-paren! height::real left::real top::real)::void

  ;;(end-line! line-height::real)::void
  ;;(cursor-at left::real top::real)::Cursor
  )

(define-object (NullScreen)::Screen
  (define (paren-width)::real 0)

  (define (min-line-height)::real 0)
  
  (define (vertical-bar-width)::real 0)
 
  (define (clear!)::void
    (values))
  
  (define (translate! x::real y::real)::void
    (values))
  
  (define (draw-string! s::string left::real top::real)::Extent
    (string-extent s))
  
  (define (draw-text! s::string left::real top::real)::real
    (string-length s))
  
  (define (draw-atom! text::string)::Extent
    (Extent width: (string-length text) height: 1))

  (define (draw-finger! left::real top::real index::byte)::Extent
   (Extent width: 1 height: 1))

  (define (draw-horizontal-bar! width::real)::void
   (values))
  
  (define (draw-vertical-bar! height::real)::void
   (values))
  
  (define (open-paren! height::real left::real top::real)::void
    (values))
  
  (define (close-paren! height::real left::real top::real)::void
   (values))

  )

(define-constant current-screen::parameter[Screen]
  (make-parameter (NullScreen)))
