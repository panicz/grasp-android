(import (define-type))
(import (primitive))
(import (indexable))
(import (space))
(import (cursor))
(import (painter))
(import (extent))
(import (match))
(import (infix))
(import (tile))

(define-type (Over back: Tile front: Tile)
  implementing Tile
  with
  ((draw! context::Cursor)
   ::void
   (let* ((back-context (recons 'back context))
          (front-context (recons 'front context)))
     (back:draw! back-context)
     (front:draw! front-context)))

  ((extent)::Extent
   (let ((front-extent (front:extent))
	 (back-extent (back:extent)))
     (Extent width: (max front-extent:width
			 back-extent:width)
	     height: (max front-extent:height
			  back-extent:height))))

  ((part-at index::Index)::Tile
   (match index
     (,(first-index) back)
     (,(last-index) front)))

  ((first-index)::Index
   'back)

  ((last-index)::Index
   'front)
  
  ((next-index index::Index)::Index
   (last-index))
   
  ((previous-index index::Index)::Index
   (first-index))

  ((index< a::Index b::Index)::boolean
   (and (is a eq? (first-index))
	(isnt b eq? (first-index))))
  )

(define-type (Below top: Tile bottom: Tile)
  implementing Tile
  with  
  ((draw! context::Cursor)
   ::void
   (let ((top-context (recons 'top context))
         (bottom-context (recons 'bottom context))
	 (top-extent (top:extent)))
     (top:draw! top-context)
     (with-translation (0 top-extent:height)
       (bottom:draw! bottom-context))))

  ((extent)::Extent
   (let ((top-extent (top:extent))
	 (bottom-extent (bottom:extent)))
     (Extent width: (max top-extent:width
			 bottom-extent:width)
	     height: (+ top-extent:height
			bottom-extent:height))))

  ((part-at index::Index)::Tile
   (match index
     (,(first-index) top)
     (,(last-index) bottom)))

  ((first-index)::Index
   'top)

  ((last-index)::Index
   'bottom)
  
  ((next-index index::Index)::Index
   (last-index))
   
  ((previous-index index::Index)::Index
   (first-index))

  ((index< a::Index b::Index)::boolean
   (and (is a eq? (first-index))
	(isnt b eq? (first-index))))
  
  )


(define-type (Beside left: Tile right: Tile)
  implementing Tile
  with
  ((draw! context::Cursor)
   ::void
   (let ((left-context (recons 'left context))
         (right-context (recons 'right context))
	 (left-extent (left:extent)))
     (left:draw! left-context)
     (with-translation (left-extent:width 0)
       (right:draw! right-context))))

  ((extent)::Extent
   (let ((left-extent (left:extent))
	 (right-extent (right:extent)))
     (Extent width: (+ left-extent:width
		       right-extent:width)
	     height: (max left-extent:height
			  right-extent:height))))
  
  ((part-at index::Index)::Tile
   (match index
     (,(first-index) left)
     (,(last-index) right)))

  ((first-index)::Index
   'left)

  ((last-index)::Index
   'right)
  
  ((next-index index::Index)::Index
   (last-index))
   
  ((previous-index index::Index)::Index
   (first-index))

  ((index< a::Index b::Index)::boolean
   (and (is a eq? (first-index))
	(isnt b eq? (first-index))))

  )
