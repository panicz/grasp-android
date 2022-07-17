(import (define-interface))
(import (indexable))
(import (cursor))
(import (painter))
(import (extent))

;; these methods are implicitly parameterized
;; with (the-painter) and (the-cursor) parameters

(define-interface Visible ()
  (draw! context::Cursor)::void)

(define-interface Extensible ()
  (extent)::Extent)

(define-interface Box (Visible Extensible))
