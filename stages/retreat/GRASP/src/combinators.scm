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
  ((part-at index::Index final::boolean)::Tile
   (match index
     ('back back)
     ('front front)))

  ((first-index)::Index
   'back)

  ((last-index)::Index
   'front)
  
  ((next-index index::Index)::Index
   'front)
   
  ((previous-index index::Index)::Index
   'back)
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
  ((part-at index::Index final::boolean)::Tile
   (match index
     ('top top)
     ('bottom bottom)))

  ((first-index)::Index
   'top)

  ((last-index)::Index
   'bottom)
  
  ((next-index index::Index)::Index
   'bottom)
   
  ((previous-index index::Index)::Index
   'top)  
  )


(define-type (Beside left: Tile right: Tile)
  implementing Tile
  with
  ((draw! screen::Screen)::Extent
   (let* ((left-extent (left:draw! screen))
          (right-extent (with-translation screen (left-extent:width 0)
                          (right:draw! screen))))
     (Extent width: (+ left-extent:width right-extent:width)
             height: (max left-extent:height right-extent:height))))
  ((part-at index::Index final::boolean)::Tile
   (match index
     ('left left)
     ('right right)))

  ((first-index)::Index
   'left)

  ((last-index)::Index
   'right)
  
  ((next-index index::Index)::Index
   'right)
   
  ((previous-index index::Index)::Index
   'left)
  
  )
