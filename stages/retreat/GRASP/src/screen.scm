(import (define-syntax-rule))
(import (define-interface))
(import (define-type))
(import (for))
(import (examples))

(define-type (Extent width: real
                     height: real))

(define-interface Screen ()
  (paren-width)::real
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
  )

(define-interface Tile ()
  (draw! screen::Screen)::Extent
  )

;; trzeba by bylo tak zrobic, zeby komorki "cons"
;; oraz symbole implementowaly interfejs Tile

(define-type (Over back: Tile front: Tile)
  implementing Tile
  with
  ((draw! screen::Screen)::Extent
   (let* ((back-extent (back:draw! screen))
          (front-extent (front:draw! screen)))
     (Extent width: (max front-extent:width
                         back-extent:width)
             height: (max front-extent:height
                          back-extent:height)))))

(define-type (Below top: Tile bottom: Tile)
  implementing Tile
  with
  ((draw! screen::Screen)::Extent
   (let* ((top-extent (top:draw! screen))
          (bottom-extent (with-translation screen (0 top-extent:height)
                           (bottom:draw! screen))))
     (Extent width: (max top-extent:width bottom-extent:width)
             height: (+ top-extent:height bottom-extent:height)))))

(define-type (Beside left: Tile right: Tile)
  implementing Tile
  with
  ((draw! screen::Screen)::Extent
   (let* ((left-extent (left:draw! screen))
          (right-extent (with-translation screen (left-extent:width 0)
                          (right:draw! screen))))
     (Extent width: (+ left-extent:width right-extent:width)
             height: (max left-extent:height right-extent:height)))))

(define-type (Finger left: real
                     top: real
                     index: byte)
  implementing Tile
  with
  ((draw! screen::Screen)::Extent
   (let ((finger (screen:draw-finger! left top index)))
     (Extent width: (+ left finger:width)
             height: (+ top finger:height)))))

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

;;(define (string-skip-first-line s::string)::string
  
(define-simple-class NullScreen (Screen)
  ((paren-width)::real 0)

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
