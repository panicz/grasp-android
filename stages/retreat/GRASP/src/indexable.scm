(import (srfi :11))
(import (srfi :17))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (define-property))
(import (default-value))
(import (define-parameter))
(import (match))
(import (infix))
(import (assert))
(import (for))
(import (examples))
(import (define-cache))
(import (print))
(import (string-building))
(import (functions))
(import (extent))
(import (fundamental))

;; the methods provided by these interfaces should be thought of as
;; implicitly parameterized with (the-painter) and (the-cursor)
;; parameters

(define-interface Indexable ()
  (part-at index::Index)::Indexable*
  
  (first-index)::Index
  (last-index)::Index
  
  (next-index index::Index)::Index
  (previous-index index::Index)::Index
  
  (index< a::Index b::Index)::boolean
  )

(define-interface Element (Indexable)
  (draw! context::Cursor)::void
  (cursor-under* x::real y::real path::Cursor)::Cursor*
  )

(define-object (Simple)::Element
  (define (typename)::String "Simple")
  (define (part-at index::Index)::Indexable* (this))
  
  (define (first-index)::Index 0)
  (define (last-index)::Index 0)
  
  (define (next-index index::Index)::Index 0)
  (define (previous-index index::Index)::Index 0)

  (define (index< a::Index b::Index)::boolean #f)

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    #!abstract)
  
  (Base))

(define-interface Tile (Element)
  (extent)::Extent)
