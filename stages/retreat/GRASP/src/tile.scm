(import (define-interface))
(import (indexable))
(import (cursor))
(import (screen))
(import (extent))

(define-interface Tile (Indexable)
  (draw! screen::Screen
	 cursor::Cursor
	 context::Cursor
	 anchor::Cursor)::void
	 
  (extent screen::Screen)::Extent
  )

(define (extent object #!optional
		(screen::Screen (current-screen)))
  ::Extent
  (cond ((instance? object Tile)
	 (invoke (as Tile object) 'extent screen))

	((null? object)
	 (Extent width: 0 height: (screen:min-line-height)))
	
	(else
	 (error "Don't know how to compute extent of "object))))
