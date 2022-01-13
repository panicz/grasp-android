(import (define-syntax-rule))
(import (define-interface))
(import (define-type))
(import (for))
(import (examples))
(import (extent))
(import (string-extras))
(import (cursor))

;; Each tile can choose whatever it pleases to be its index.
;; For built-in types (boxes, combinators, atoms) indices are
;; typically either integers or characters or symbols
(define-alias Index java.lang.Object)

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

(define-interface Tile ()
  (draw! screen::Screen cursor::Cursor context::Cursor)::Extent

  ;; for pairs with null head or null tail, the `final` variable
  ;; is used to decide whether we should return null-head/tail-space
  ;; or just the empty list (which, unlike cons cells, has no instance
  ;; identity)
  (part-at index::Index final::boolean)::Tile
  
  (first-index)::Index
  (last-index)::Index
  
  (next-index index::Index)::Index
  (previous-index index::Index)::Index
  )

(define-type (Finger left: real
                     top: real
                     index: byte)
  implementing Tile
  with
  ((draw! screen::Screen cursor::Cursor context::Cursor)::Extent
   (let ((finger (screen:draw-finger! left top index)))
     (Extent width: (+ left finger:width)
             height: (+ top finger:height))))
  ((part-at index::Index final::boolean)::Tile
   #!null)
  
  ((first-index)::Index
   #!null)
  
  ((last-index)::Index
   #!null)
  
  ((next-index index::Index)::Index
   #!null)
  
  ((previous-index index::Index)::Index
   #!null)
  
  )

(define-simple-class NullScreen (Screen)
  ((paren-width)::real 0)

  ((min-line-height)::real 0)
  
  ((vertical-bar-width)::real 0)
 
  ((clear!)::void
   (values))
  
  ((translate! x::real y::real)::void
   (values))
  
  ((draw-string! s::string left::real top::real)::Extent
   (string-extent s))
  
  ((draw-text! s::string left::real top::real)::real
   (string-length s))
  
  ((draw-atom! text::string)::Extent
   (Extent width: (string-length text) height: 1))

  ((draw-finger! left::real top::real index::byte)::Extent
   (Extent width: 1 height: 1))

  ((draw-horizontal-bar! width::real)::void
   (values))
  
  ((draw-vertical-bar! height::real)::void
   (values))
  
  ((open-paren! height::real left::real top::real)::void
   (values))
  
  ((close-paren! height::real left::real top::real)::void
   (values))
  )

(define-constant current-screen::parameter[Screen]
  (make-parameter (NullScreen)))

(define-syntax-rule (with-translation screen (x y) . actions)
  (let ((x! x)
        (y! y))
    (screen:translate! x! y!)
    (let ((result (begin . actions)))
      (screen:translate! (- x!) (- y!))
      result)))
