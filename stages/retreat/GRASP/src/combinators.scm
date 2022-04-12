(import (define-type))
(import (primitive))
(import (indexable))
(import (screen))
(import (extent))
(import (match))
(import (infix))

(define-type (Over back: Tile front: Tile)
  implementing Tile
  with
  ((draw! screen::Screen
	  cursor::Cursor
	  context::Cursor
	  anchor::Cursor)
   ::void
   (let* ((back-context (recons 'back context))
          (front-context (recons 'front context)))
     (back:draw! screen
                 cursor
                 back-context
		 anchor)
     (front:draw! screen
		  cursor
                  front-context
		  anchor)))

  ((extent screen::Screen)::Extent
   (let ((front-extent (front:extent screen))
	 (back-extent (back:extent screen)))
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

  ((send-char! c::char cursor::Cursor level::int)::Cursor
   (if (is level < 0)
       cursor
       (let* ((index (cursor level))
	      (part (part-at index)))
	 (send-char-to! part c cursor (- level 1)))))
  ((deletable?)::boolean #t)
  )

(define-type (Below top: Tile bottom: Tile)
  implementing Tile
  with  
  ((draw! screen::Screen
	  cursor::Cursor
	  context::Cursor
	  anchor::Cursor)
   ::void
   (let ((top-context (recons 'top context))
         (bottom-context (recons 'bottom context))
	 (top-extent (top:extent screen)))
     (top:draw! screen
		cursor
                top-context
		anchor)
     (with-translation screen (0 top-extent:height)
       (bottom:draw! screen
		     cursor
                     bottom-context
		     anchor))))

  ((extent screen::Screen)::Extent
   (let ((top-extent (top:extent screen))
	 (bottom-extent (bottom:extent screen)))
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
  
  ((send-char! c::char cursor::Cursor level::int)::Cursor
   (if (is level < 0)
       cursor
       (let* ((index (cursor level))
	      (part (part-at index)))
	 (send-char-to! part c cursor (- level 1)))))
  ((deletable?)::boolean #t)
  )


(define-type (Beside left: Tile right: Tile)
  implementing Tile
  with
  ((draw! screen::Screen
	  cursor::Cursor
	  context::Cursor
	  anchor::Cursor)
   ::void
   (let ((left-context (recons 'left context))
         (right-context (recons 'right context))
	 (left-extent (left:extent screen)))
     (left:draw! screen
		 cursor
                 left-context
		 anchor)
     (with-translation screen (left-extent:width 0)
       (right:draw! screen
		    cursor
                    right-context
		    anchor))))

  ((extent screen::Screen)::Extent
   (let ((left-extent (left:extent screen))
	 (right-extent (right:extent screen)))
     (Extent width: (+ left-extent:width
		       right-extent:width screen)
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

  ((send-char! c::char cursor::Cursor level::int)::Cursor
   (if (is level < 0)
       cursor
       (let* ((index (cursor level))
	      (part (part-at index)))
	 (send-char-to! part c cursor (- level 1)))))
  ((deletable?)::boolean #t)
  )
