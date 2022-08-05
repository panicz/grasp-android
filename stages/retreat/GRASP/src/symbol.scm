(import (define-object))
(import (fundamental))
(import (indexable))
(import (cursor))
(import (painter))
(import (infix))
(import (extent))
(import (functions))

(define-object (Symbol source::string)::Tile
  (define builder :: java.lang.StringBuilder)
  
  (define (draw! context::Cursor)
    ::void
    (invoke (the-painter) 'draw-atom! name
	    (and (pair? (the-cursor))
		 (equal? (cursor-tail) context)
		 (cursor-head))))

  (define (extent)::Extent
    (invoke (the-painter) 'atom-extent name))
  
  (define (part-at index::Index)::Indexable*
    (this))

  (define (first-index)::Index
    0)
  
  (define (last-index)::Index
    (string-length name))
  
  (define (next-index index::Index)::Index
    (min (last-index) (+ index 1)))
  
  (define (previous-index index::Index)::Index
    (max 0 (- index 1)))

  (define (index< a::Index b::Index)::boolean
    (and (number? a) (number? b)
	 (is a < b)))

  (define (insert-char! c::char index::int)::void
    (builder:insert index c)
    (set! name ((builder:toString):intern)))

  (define (delete-char! index::int)::void
    (builder:deleteCharAt index)
    (set! name ((builder:toString):intern)))

  (define (truncate! length::int)::void
    (builder:setLength length)
    (set! name ((builder:toString):intern)))

  (define (subpart start::int)::Symbol
    (Symbol (invoke name 'substring start)))
  
  (gnu.mapping.SimpleSymbol
   ((source:toString):intern))
  (set! builder (java.lang.StringBuilder name)))

(define (symbol-length s::Symbol)::int
  (invoke (slot-ref s 'builder) 'length))

(define (insert-char! c::char s::Symbol index::int)::void
  (invoke s 'insert-char! c index))

(define (delete-char! s::Symbol index::int)::void
  (invoke s 'delete-char! index))

(define (truncate-symbol! s::Symbol length::int)::void
  (invoke s 'truncate! length))

(define (symbol-subpart s::Symbol start::int)::Symbol
  (invoke s 'subpart start))
