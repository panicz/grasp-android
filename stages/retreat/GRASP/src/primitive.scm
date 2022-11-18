
(import (srfi :11))
(import (define-syntax-rule))
(import (assert))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (define-property))
(import (define-cache))
(import (default-value))
(import (define-parameter))
(import (match))
(import (examples))
(import (infix))
(import (fundamental))
(import (indexable))
(import (extent))
(import (space))
(import (cursor))
(import (for))
(import (painter))
(import (functions))
(import (print))
(import (conversions))
(import (traversal))

;; we override Pair with Object's default equality and hash functions
;; (TODO: patch the Kawa implementation of Cons)

;;  0 1 2 3 4 5 6

;; ( car  .  cdr )

;;  0 1 2  3 4  5  6 7 8  9  10
;; ( car cadr caddr  .  cdddr  )

;;  0 1 2
;; (  x  )


;; The two modes of operation are "editing mode"
;; and "evaluating mode". The difference is in the
;; treatment of Atoms: when we are (evaluating?),
;; Atoms behave as if they were transparent,
;; so that we can only see their (value).
;; But if we're not (evaluating?), then we can
;; see (and operate on) Atoms themselves.
(define-parameter (evaluating?) ::boolean #f)

(define (editing?) ::boolean
  (not (evaluating?)))

;; The purpose of Atoms is to solve the problem that
;; the actual atomic Scheme values have different
;; types (e.g. the result of reading "1" is a number,
;; but the result of reading "1+" is a symbol, and
;; the result of reading "1+:" is a keyword).
;; Atoms therefore provide an identity for the
;; edited objects, even though the "value" of
;; those atoms can be a different kind of object
;; on every query.
(define-object (Atom source-string::String)::Tile
  (define builder :: java.lang.StringBuilder)
  (define source :: String "")
  
  (define cache #!null)
  
  (define (value)
    (or cache
	(let ((result (call-with-input-string source read)))
	  (set! cache result)
	  result)))
  
  (define (draw! context::Cursor)
    ::void
    (invoke (the-painter) 'draw-atom! source
	    context))
  
  (define (extent)::Extent
    (invoke (the-painter) 'atom-extent source))
  
  (define (part-at index::Index)::Indexable*
    (this))

  (define (first-index)::Index
    0)
  
  (define (last-index)::Index
    (string-length source))
  
  (define (next-index index::Index)::Index
    (min (last-index) (+ index 1)))
  
  (define (previous-index index::Index)::Index
    (max 0 (- index 1)))

  (define (index< a::Index b::Index)::boolean
    (and (number? a) (number? b)
	 (is a < b)))

  (define (insert-char! c::char index::int)::void
    (builder:insert index c)
    (set! cache #!null)
    (set! source ((builder:toString):intern)))

  (define (delete-char! index::int)::void
    (builder:deleteCharAt index)
    (set! cache #!null)
    (set! source ((builder:toString):intern)))

  (define (truncate! length::int)::void
    (builder:setLength length)
    (set! cache #!null)
    (set! source ((builder:toString):intern)))

  (define (subpart start::int)::Atom
    (Atom (source:substring start)))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (let ((inner (extent))
	  (painter (the-painter)))
      (and (is 0 <= x < inner:width)
	   (is 0 <= y < inner:height)
	   (recons (painter:atom-character-index-under x y
						       source)
		   path)
	   )))
  
  (define (toString)::String
    source)
  
  (set! builder (java.lang.StringBuilder source-string))
  (set! source (builder:toString)))

(define (atom-length a::Atom)::int
  (a:builder:length))

(define (insert-char! c::char a::Atom index::int)::void
  (a:insert-char! c index))

(define (delete-char! a::Atom index::int)::void
  (a:delete-char! index))

(define (truncate-atom! a::Atom length::int)::void
  (a:truncate! length))

(define (atom-subpart a::Atom start::int)::Atom
  (a:subpart start))


(define-object (cons a d)::Tile
  (define (equals object)::boolean
   (eq? object (this)))

  (define (hashCode)::int
    (java.lang.System:identityHashCode (this)))

  (define (draw! context::Cursor)
    ::void
    (let* ((inner (sequence-extent (this)))
	   (painter (the-painter))
	   (paren-width (painter:paren-width)))
      (painter:draw-box! (+ inner:width (* 2 paren-width))
			 inner:height
			 context)
      (with-translation (paren-width 0)
	  (draw-sequence! (this) context: context))))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (let ((inner (sequence-extent (this)))
	  (paren-width (invoke (the-painter) 'paren-width)))

      (and (is 0 <= y < inner:height)
	   (or (and (is 0 <= x < paren-width)
		    (recons (first-index) path))
	       
	       (and (is 0 <= (- x paren-width) < inner:width)
		    (cursor-under (- x paren-width) y
				  (this) context: path))
	       (and (is 0 <= (- x paren-width inner:width)
			< paren-width)
		    (recons (last-index) path))))))
  
  (define (extent)::Extent
    (let ((extent ::Extent (sequence-extent
			    (this))))
      (Extent width: (+ extent:width
			(* 2 (invoke (the-painter)
				     'paren-width)))
	      height: extent:height)))
  
  (define (part-at index::Index)::Indexable*
    (if (or (eq? index (first-index))
	    (eq? index (last-index)))
	(this)
	(cell-index (this) (as int index))))
  
  (define (first-index)::Index
    #\[)
  
  (define (last-index)::Index
    #\])
  
  (define (next-index index::Index)::Index
    (match index
      (,(first-index) 0)
      (,(last-index) (last-index))
      (,@(is _ < (last-cell-index (this)))
       (+ index 1))
      (_
       (last-index))))
  
  (define (previous-index index::Index)::Index
    (match index
      (0 (first-index))
      (,(last-index) (last-cell-index (this)))
      (,(first-index) (first-index))
      (_ (- index 1))))

  (define (index< a::Index b::Index)::boolean
    (or (and (is a eqv? (first-index))
	     (isnt b eqv? (first-index)))
	(and (number? a) (number? b)
	     (is a < b))
	(and (is b eqv? (last-index))
	     (isnt a eqv? (last-index)))))

  (define (getCar)
    (let ((element (invoke-special pair (this) 'getCar)))
      (if (and (evaluating?)
	       (is element instance? Atom))
	  (invoke (as Atom element) 'value)
	  element)))

  (define (getCdr)
    (let ((element (invoke-special pair (this) 'getCdr)))
      (if (and (evaluating?)
	       (is element instance? Atom))
	  (invoke (as Atom element) 'value)
	  element)))

  (pair a d))

(define-object (immutable-cons a d)::Tile

  (define (setCar value)
    (error "The cons cell is immutable: " (this)))

  (define (setCdr value)
    (error "The cons cell is immutable: "(this)))
  
  (cons a d))

(define-cache (recons head tail)
  (immutable-cons head tail))

(define-syntax cons*
  (syntax-rules ()
    ((_ a b)
     (cons a b))

    ((_ a b c ...)
     (cons a (cons* b c ...)))))

(define-syntax recons*
  (syntax-rules ()
    ((_ a b)
     (recons a b))
    
    ((_ a b c ...)
     (recons a (recons* b c ...)))))

(define-syntax cursor
  (syntax-rules ()
    ((_) '())
    ((_ indices ...)
     (recons* indices ... '()))))

(define (empty-space-extent space::Space)
  ::Extent
  (Extent width: (apply max space:fragments)
	  height: (* (invoke (the-painter) 'min-line-height)
		     (length space:fragments))))

(define (head-extent pair::cons)
  ::Extent
  (if (null? (head pair))
      (let ((inner (empty-space-extent
		    (null-head-space pair))))
	(Extent width: (+ inner:width
			  (* 2 (invoke (the-painter)
				       'paren-width)))
		height: inner:height))
      (extent (head pair))))

(define (tail-extent pair::cons)
  ::Extent
  (if (null? (tail pair))
      (let ((inner (empty-space-extent
		    (null-head-space pair))))
	(Extent width: (+ inner:width
			  (* 2 (invoke (the-painter)
				       'paren-width)))
		height: inner:height))
      (extent (tail pair))))

(define (advance-traversal! traversal::Traversal
			    element::Element)
  ::void
  (cond ((is element Space?)
	 (let ((space ::Space (as Space element)))
	   (space:advance! traversal)))
	((is element Tile?)
	 (let ((tile ::Tile (as Tile element)))
           (traversal:advance/extent! (tile:extent))))))

(define (traverse sequence::list
		  #!key
		  (doing nothing)
		  (returning nothing))
  (let* ((painter (the-painter))
         (traversal (Traversal
		     max-line-height:
		     (painter:min-line-height))))

    (parameterize ((the-traversal traversal))
      
      (define (head* pair::pair)::Tile
	(if (null? (head pair))
            (empty-list-proxy (null-head-space pair))
            (head pair)))

      (define (tail* pair::pair)::Tile
	(if (null? (tail pair))
            (empty-list-proxy (null-tail-space pair))
            (tail pair)))

      (define (step-over-dotted-tail! pair::pair)::void
	(let* ((horizontal? (should-the-bar-be-horizontal? pair))
               (bar (if horizontal?
			(horizontal-bar traversal:max-width)
			(vertical-bar traversal:max-line-height)))
               (pre-tail (if horizontal?
                             (skip-first-line
			      (pre-tail-space pair))
                             (pre-tail-space pair)))
               (item (tail* pair))
	       (post-tail (post-tail-space pair)))
          (doing bar traversal)
          (advance-traversal! traversal bar)
	  (when horizontal?
	    (set! traversal:left 0))
	  (doing pre-tail traversal)
          (advance-traversal! traversal pre-tail)
          (doing item traversal)
          (advance-traversal! traversal item)
          (doing post-tail traversal)
          (advance-traversal! traversal post-tail)))

      (define (step! pair::pair)
	(let ((item (head* pair))
              (post-head (post-head-space pair)))
          (doing item traversal)
          (advance-traversal! traversal item)
          (doing post-head traversal)
          (advance-traversal! traversal post-head)
          (cond ((dotted? pair)
		 (step-over-dotted-tail! pair)
		 (returning traversal))
		((pair? (tail pair))
		 (step! (tail pair)))
		(else
		 (returning traversal)))))

      (if (pair? sequence)
	  (let ((pre-head (pre-head-space sequence)))
            (doing pre-head traversal)
            (advance-traversal! traversal pre-head)
            (step! sequence))
	  (returning traversal))
      )))

(define (draw-sequence! #!optional
			(elems::list (head (the-document)))
			#!key (context::Cursor (recons 1 '())))
  ::void
  (let-values (((selection-start selection-end) (the-selection)))
    (let ((painter (the-painter)))
      (traverse
       elems
       doing:
       (lambda (item::Element traversal::Traversal)
	 (with-translation (traversal:left
			    traversal:top)
	     (let ((context (recons traversal:index
				    context)))
	       (when (equal? context selection-start)
		 (painter:enter-selection-drawing-mode!))
	       (item:draw! context)
	       (when (equal? context selection-end)
		 (painter:exit-selection-drawing-mode!)))))))))

(define (draw-document! document::pair)
  (cond ((null? (head document))
	 (draw! (null-head-space document)))
	((pair? (head document))
	 (draw! (pre-head-space (head document)))
	 (draw-sequence! (head document)))))

(define (draw! object #!key
	       (context::Cursor '()))
  ::void
  (cond ((instance? object Element)
	 (invoke (as Element object)
		 'draw! context))

	((null? object)
	 (values))

	(else
	 (with-translation (0 1)
	     (invoke (the-painter)
		     'draw-string!
		     (with-output-to-string
		       (lambda () (write object)))
		     (and (pair? (the-cursor))
			  (equal? (cdr (the-cursor)) context)
			  (car (the-cursor))))))))

(define (cursor-under left::real top::real
		      #!optional
		      (elems::list (head (the-document)))
		      #!key (context::Cursor (recons 1 '())))
  ::Cursor
  (call/cc
   (lambda (return)
     (traverse
      elems
      doing:
      (lambda (item::Element t::Traversal)
	(and-let* ((cursor (item:cursor-under*
			    (- left t:left)
			    (- top t:top)
			    (recons t:index context))))
	  (return cursor)))
      returning: (lambda (t::Traversal)
		   context)))))

#|
(define (cursor-above cursor::Cursor
		      #!optional
		      (document::list (head (the-document))))
  ::Cursor
  ...)

(define (cursor-below cursor::Cursor
		      #!optional
		      (document::list (head (the-document)))
		      #!key (context::Cursor (recons 1 '())))
  ::Cursor
  (call/cc
   (lambda (return)
     (traverse
      document
      doing:
      (lambda (item::Element t::Traversal)
	...)))))

|#

(define (sequence-extent #!optional
			 (elems::list (head (the-document))))
  ::Extent
  (traverse elems
	    returning:
	    (lambda (traversal::Traversal)
	      (Extent width: traversal:max-width
		      height: (+ traversal:top
				 traversal:max-line-height)))))

(define (extent object)
  ::Extent
  (cond ((instance? object Tile)
	 (invoke (as Tile object) 'extent))

	((null? object)
	 (Extent width: 0 height: (invoke (the-painter)
					  'min-line-height)))

	((pair? object)
	 (sequence-extent object))

	((symbol? object)
	 (invoke (the-painter) 'atom-extent
		 (symbol->string object)))
	
	(else
	 (invoke (the-painter) 'atom-extent
		 (with-output-to-string
		   (lambda () (write object)))))
	))
