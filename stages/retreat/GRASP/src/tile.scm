(import (define-interface))
(import (indexable))
(import (cursor))
(import (screen))
(import (extent))

(define-interface Tile (Indexable)
  ;; these methods are implicitly parameterized
  ;; with (the-screen) and (the-cursor) parameters
  (draw! context::Cursor)
  ::void
	 
  (extent)::Extent)

(define Tile* java.lang.Object)
