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

(define (draw! object #!key
               (screen::Screen (current-screen))
               (cursor::Cursor '())
	       (context::Cursor '())
	       (anchor::Cursor '()))
  ::void
  (cond ((instance? object Tile)
	 (invoke (as Tile object)
		 'draw! screen cursor context anchor))

	(else
	 (error "Don't know how to draw "object))))

(define (extent object #!optional
		(screen::Screen (current-screen)))
  ::Extent
  (cond ((instance? object Tile)
	 (invoke (as Tile object) 'extent screen))

	(else
	 (error "Don't know how to draw "object))))
