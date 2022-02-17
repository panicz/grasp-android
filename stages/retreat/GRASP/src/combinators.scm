(import (define-type))
(import (extent))
(import (primitive))
(import (indexable))
(import (screen))

(define-type (Over back: Tile front: Tile)
  implementing Tile
  with
  ((draw! screen::Screen cursor::Cursor context::Cursor)::Extent
   (let* ((back-context (recons 'back context))
          (front-context (recons 'front context))
          (back-extent (back:draw! screen
                                   (subcursor cursor back-context)
                                   back-context))
          (front-extent (front:draw! screen
                                     (subcursor cursor front-context)
                                     front-context)))
     (Extent width: (max front-extent:width
                         back-extent:width)
             height: (max front-extent:height
                          back-extent:height))))
  ((has-children?)::boolean #t)
  
  ((part-at index::Index)::Tile
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
  ((draw! screen::Screen cursor::Cursor context::Cursor)::Extent
   (let* ((top-context (recons 'top context))
          (bottom-context (recons 'bottom context))
          (top-extent (top:draw! screen (subcursor cursor top-context)
                                 top-context))
          (bottom-extent (with-translation screen (0 top-extent:height)
                           (bottom:draw! screen (subcursor cursor bottom-context)
                                         bottom-context))))
     (Extent width: (max top-extent:width bottom-extent:width)
             height: (+ top-extent:height bottom-extent:height))))

  ((has-children?)::boolean #t)

  ((part-at index::Index)::Tile
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
  ((draw! screen::Screen cursor::Cursor context::Cursor)::Extent
   (let* ((left-context (recons 'left context))
          (right-context (recons 'right context))
          (left-extent (left:draw! screen (subcursor cursor left-context)
                                   left-context))
          (right-extent (with-translation screen (left-extent:width 0)
                          (right:draw! screen (subcursor cursor right-context)
                                       right-context))))
     (Extent width: (+ left-extent:width right-extent:width)
             height: (max left-extent:height right-extent:height))))
  ((has-children?)::boolean #t)
  
  ((part-at index::Index)::Tile
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
