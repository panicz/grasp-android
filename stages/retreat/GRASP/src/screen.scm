(import (define-syntax-rule))
(import (define-interface))
(import (define-type))
(import (for))
(import (examples))
(import (match))

(define-alias List gnu.lists.LList)

(define-type (Extent width: real
                     height: real))

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
  (draw! screen::Screen #|context::List cursor::Cursor|#)::Extent

  ;; for pairs with null head or null tail, the `final` variable
  ;; is used to decide whether we should return null-head/tail-space
  ;; or just the empty list (which, unlike cons cells, has no instance
  ;; identity)
  (part-at index::int final::boolean)::Tile
  (subindices)::int
  )

;; A Cursor is a list of non-negative integers. Each integer
;; corresponds to the index at a given level, where the first
;; index refers to the innermost expression's context, and the
;; last index corresponds to the outermost expression.
;;
;; This order doesn't allow to select the expression pointed to by
;; a cursor in a tail-recursive manner: we need to reach the last
;; index in order to choose a sub-expression on a top-level.
;;
;; The reason we chose this "reverse" order has to do with
;; the way we build those indices: we start from a top level,
;; and we descend deeper recursively; therefore, we "cons"
;; the inermost expressions' indices last.
;;
;; Also, this strategy maximizes "structural sharing"
;; between cursors to different expressions
;; (which I think is beautiful), and reverting this
;; order would be wasteful
;;
;; Another thing with cursors is that, when refering to
;; normal boxes,  even indices usually refer to spaces,
;; and odd indices refer to subsequent elements in a list.
;;
;; The exception is in the case of a dotted tail:
;; the odd index refers to the tail itself, as if it was
;; an element, and the next odd index refers to the
;; cdr of the list.
(define-alias Cursor gnu.lists.LList)

(define (part-at cursor::Cursor tile::Tile)::Tile
  (match cursor
    ('() tile)
    (`(,index . ,indices)
     ((part-at indices tile):part-at index
      (null? indices)))
    (_ #!null)))

(define-type (Over back: Tile front: Tile)
  implementing Tile
  with
  ((draw! screen::Screen)::Extent
   (let* ((back-extent (back:draw! screen))
          (front-extent (front:draw! screen)))
     (Extent width: (max front-extent:width
                         back-extent:width)
             height: (max front-extent:height
                          back-extent:height))))
  ((part-at index::int final::boolean)::Tile
   (if (= index 0)
       back
       front))
  ((subindices)::int
   2)
  )

(define-type (Below top: Tile bottom: Tile)
  implementing Tile
  with
  ((draw! screen::Screen)::Extent
   (let* ((top-extent (top:draw! screen))
          (bottom-extent (with-translation screen (0 top-extent:height)
                           (bottom:draw! screen))))
     (Extent width: (max top-extent:width bottom-extent:width)
             height: (+ top-extent:height bottom-extent:height))))
  ((part-at index::int final::boolean)::Tile
   (if (= index 0)
       top
       bottom))
  ((subindices)::int
   2))


(define-type (Beside left: Tile right: Tile)
  implementing Tile
  with
  ((draw! screen::Screen)::Extent
   (let* ((left-extent (left:draw! screen))
          (right-extent (with-translation screen (left-extent:width 0)
                          (right:draw! screen))))
     (Extent width: (+ left-extent:width right-extent:width)
             height: (max left-extent:height right-extent:height))))
  ((part-at index::int final::boolean)::Tile
   (if (= index 0)
       left
       right))
  ((subindices)::int
   2))

(define-type (Finger left: real
                     top: real
                     index: byte)
  implementing Tile
  with
  ((draw! screen::Screen)::Extent
   (let ((finger (screen:draw-finger! left top index)))
     (Extent width: (+ left finger:width)
             height: (+ top finger:height))))
  ((part-at index::int final::boolean)
   #!null)
  ((subindices)::int
   0))

(define (string-extent s::string)::Extent
  (let ((line-length 0)
        (max-length 0)
        (total-lines 1))
    (for c in s
         (cond ((eq? c #\newline)
                (set! max-length (max max-length line-length))
                (set! total-lines (+ total-lines 1))
                (set! line-length 0))
               (else
                (set! line-length (+ line-length 1)))))
    (Extent width: (max max-length line-length)
            height: total-lines)))

(e.g.
 (string-extent "\
abc
def") ===> [Extent width: 3 height: 2])

(define (string-last-line s::string)::string
  (let ((n (string-length s))
        (last-newline 0))
    (for i from 0 below n
         (if (eq? (string-ref s i) #\newline)
             (set! last-newline (+ i 1))))
    (substring s last-newline n)))

(e.g.
 (string-last-line "\
abc
def") ===> "def")

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
