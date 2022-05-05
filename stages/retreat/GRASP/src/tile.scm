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

