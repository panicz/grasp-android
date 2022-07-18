(import (define-interface))
(import (indexable))
(import (cursor))
(import (painter))
(import (extent))

(define-interface Box ()
  ;; these methods are implicitly parameterized
  ;; with (the-painter) and (the-cursor) parameters
  (draw! context::Cursor)::void
  (extent)::Extent
  )
