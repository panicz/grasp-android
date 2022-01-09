(import (screen))
(import (define-type))
(import (extent))

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
   2))

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
