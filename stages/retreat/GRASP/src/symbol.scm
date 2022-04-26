(import (define-object))
(import (indexable))
(import (cursor))
(import (screen))
(import (tile))
(import (infix))
(import (extent))
(import (functions))

(define-object (Symbol source::string)::Tile
  (define builder :: java.lang.StringBuilder)
  
  (define (draw! screen::Screen
		 cursor::Cursor
		 context::Cursor
		 anchor::Cursor)
    ::void
    (screen:draw-atom! name)
    (when (and (pair? cursor)
	       (equal? (tail cursor) context))
      (let ((index (head cursor)))
	(screen:remember-offset! index 2))))

  (define (extent screen::Screen)::Extent
    (Extent width: (screen:atom-width name)
	    height: (screen:min-line-height)))
  
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

  (define (insert-char! c::char index::int)
    (builder:insert index c)
    (set! name ((builder:toString):intern)))

  (define (delete-char! index::int)
    (builder:deleteCharAt index)
    (set! name ((builder:toString):intern)))
    
  (gnu.mapping.SimpleSymbol
   ((source:toString):intern))
  (set! builder (java.lang.StringBuilder name)))

(define (symbol-length s::Symbol)
  (invoke (slot-ref s 'builder) 'length))

(define (insert-char! c::char s::Symbol index::int)
  (invoke s 'insert-char! c index))

(define (delete-char! s::Symbol index::int)
  (invoke s 'delete-char! index))
