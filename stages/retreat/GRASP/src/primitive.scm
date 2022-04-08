(import (define-syntax-rule))
(import (assert))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (define-property))
(import (define-cache))
(import (match))
(import (examples))
(import (infix))
(import (extent))
(import (indexable))
(import (for))
(import (screen))
(import (functions))
(import (print))

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

(define-syntax-rule (with-translation screen (x y)
				      . actions)
  (let ((x! x)
        (y! y))
    (screen:translate! x! y!)
    (begin . actions)
    (screen:translate! (- x!) (- y!))))

;; we override Pair with Object's default equality and hash functions
;; (TODO: patch the Kawa implementation of Cons)

;;  0 1 2 3 4 5 6
;; ( car  .  cdr )

;;  0 1 2  3 4  5  6 7 8  9  10
;; ( car cadr caddr  .  cdddr  )

;;  0 1 2
;; (  x  )

(define-object (cons a d)::Tile
  (define (equals object)::boolean
   (eq? object (this)))

  (define (hash-code)::int
    (java.lang.System:identity-hash-code (this)))

  (define (draw! screen::Screen
		 cursor::Cursor
		 context::Cursor
		 anchor::Cursor)
    ::void
    (let ((inner (sequence-extent (this) screen))
	  (paren-width (screen:paren-width)))
      (screen:open-paren! inner:height 0 0)
      (when (equal? cursor (recons (first-index)
				   context))
	(screen:remember-offset! 0 2))
      (with-translation screen (paren-width 0)
	(draw-sequence! (this)
			screen: screen
			cursor: cursor
			context: context))
      (screen:close-paren! inner:height
			   (+ paren-width
			      inner:width) 0)
      (when (equal? cursor (recons (last-index)
				   context))
	(screen:remember-offset! (+ paren-width 1
				    inner:width)
				 (- inner:height
				    1)))
      ))

  (define (extent screen::Screen)::Extent
    (let ((extent ::Extent (sequence-extent
			    (this) screen)))
      (Extent width: (+ extent:width
			(* 2 (screen:paren-width)))
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
  
  (define (send-char! c::char cursor::Cursor level::int)
    ::Cursor
    (define (to-be-deleted? target::Indexable)
      (and (or (and (symbol? target)
		    (is (string-length
			 (target:toString)) = 1))
	       (pair? target))
	   (or (and (eq? c #\backspace)
		    (eqv? (head cursor)
			  (target:last-index)))
	       (and (eq? c #\delete)
		    (eqv? (head cursor)
			  (target:first-index))))))
    
    (if (is level < 0)
	cursor
	(let* ((index (cursor level))
	       (part (part-at index)))
	  (cond
	   
	   ((and-let* (((eqv? level 2))
		       ((number? index))
		       (owner-index (quotient index 2))
		       (subindex (cursor 1))
		       ((eqv? subindex 1))
		       (subpart (cell-index part
					    subindex))
		       ((to-be-deleted? subpart))
		       (parent-cell (drop owner-index
					  (this))))
	      (set! (pre-head-space
		     (tail (head parent-cell)))
		(join-spaces!
		 (pre-head-space (head parent-cell))
		 (post-head-space (head parent-cell))))
	      (set! (head parent-cell)
		(tail (head parent-cell)))
	      (recons (- subindex 1)
		      (tail (tail cursor)))))

	   ((and (eqv? level 1)
		 (number? index)
		 (is index > 1)
		 (to-be-deleted? part))
	    (let ((previous-cell (drop (- (quotient index 2)
					  1)
				       (this))))
	      (set! (post-head-space previous-cell)
		(join-spaces!
		 (post-head-space previous-cell)
		 (post-head-space (tail previous-cell))))
	      (set! (tail previous-cell)
		(tail (tail previous-cell)))
	      (recons (- index 1)
		      (tail (tail cursor)))))
	   
	   (else
	    (send-char-to! part c cursor
			   (- level 1)))))))

  (pair a d))

(define-cache (heads tail)
  (cache (head)
	 (cons head tail)))

(define (recons head tail)::cons
  ((heads tail) head))

(define-syntax recons*
  (syntax-rules ()
    ((_ a b)
     (recons a b))
    
    ((_ a b c ...)
     (recons a (recons* b c ...)))))

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

  (define (send-char! c::char cursor::Cursor level::int)
    ::Cursor
    (cond ((= level 0)
	   (let ((index ::int (head cursor)))
	     (match c
	       (#\backspace
		(cond ((is index > 0)
		       (builder:deleteCharAt (- index 1))
		       (set! name
			 ((builder:toString):intern))
		       (recons (- index 1) (tail cursor)))
		      (else
		       cursor)))
	       (#\delete
		(when (is index < (string-length name))
		  (builder:deleteCharAt index)
		  (set! name ((builder:toString):intern)))
		cursor)

	       (_
		(builder:insert index c)
		(set! name ((builder:toString):intern))
		(recons (+ index 1) (tail cursor))))))

	  (else
	   (WARN "The symbol "name" cannot support char "
		 c" at level "level)
	   cursor)))
  
  (gnu.mapping.SimpleSymbol ((source:toString):intern))
  (set! builder (java.lang.StringBuilder name)))

(define (empty-space-extent space::Space
			    screen::Screen)
  ::Extent
  (Extent width: (apply max space:fragments)
	  height: (* (screen:min-line-height)
		     (length space:fragments))))

(define (head-extent pair::cons screen::Screen)
  ::Extent
  (if (null? (head pair))
      (let ((inner (empty-space-extent
		    (null-head-space pair)
		    screen)))
	(Extent width: (+ inner:width
			  (* 2 (screen:paren-width)))
		height: inner:height))
      (extent (head pair) screen)))

(define (tail-extent pair::cons screen::Screen)
  ::Extent
  (if (null? (tail pair))
      (let ((inner (empty-space-extent
		    (null-head-space pair)
		    screen)))
	(Extent width: (+ inner:width
			  (* 2 (screen:paren-width)))
		height: inner:height))
      (extent (tail pair) screen)))

(define (skip-first-line s::Space)::Space
  (match s:fragments
    (`(,_ ,_ . ,_)
     (Space fragments: (tail s:fragments)))
    (_
     (Space fragments: '(0)))))

(define (should-the-bar-be-horizontal? dotted-pair)
  ::boolean
  (assert (dotted? dotted-pair))
  (and-let* (((Space fragments: `(,_ ,_ . ,_))
	      (post-head-space dotted-pair))
	     ((Space fragments: `(,_ ,_ . ,_))
	      (pre-tail-space dotted-pair)))))

(define (draw-sequence! elems::cons #!key
                        (screen :: Screen
				(current-screen))
                        (cursor::Cursor '())
                        (context::Cursor '())
			(anchor::Cursor))
  ::void
  (let ((max-width 0)
        (max-line-height (screen:min-line-height))
        (top 0)
        (left 0)
        (index 0))

    (define (skip-spaces! space::Space)::void
      (let skip ((input space:fragments)
		 (total 0))
	(define (advance-with-cursor! width::real)
	  (when (and-let* ((`(,tip ,next . ,sub) cursor)
			   ((integer? tip))
			   ((equal? sub context))
			   ((eqv? next index))
			   ((is total <= tip < (+ total
						  width)))))
	    (screen:remember-offset! (+ left
					(- (head cursor)
					   total)
					-1)
				     (+ top 2)))
	  (set! left (+ left width))
	  (set! max-width (max max-width left)))
	
	(match input
	  (`(,,@integer? ,,@integer? . ,_)
	   (advance-with-cursor! (head input))
	   (set! top (+ top max-line-height))
           (set! left 0)
           (set! max-line-height
	     (screen:min-line-height))
	   (skip (tail input) (+ total (head input))))
	  (`(,,@integer)
	   (advance-with-cursor! (head input)))))
      (set! index (+ index 1)))

    (define (advance! extent::Extent)::void
      (when (equal? cursor (recons index context))
	(screen:remember-offset! left (+ top 2)))
      (set! left (+ left extent:width))
      (set! max-line-height (max extent:height
				 max-line-height))
      (set! max-width (max left max-width))
      (set! index (+ index 1)))

    (define (draw-empty-list! space::Space)::void
      (let ((inner (empty-space-extent space screen))
	    (paren-width (screen:paren-width)))
	(screen:open-paren! inner:height 0 0)
	(screen:close-paren! inner:height
			     (+ paren-width
				inner:width) 0)))
    
    (define (draw-head! pair::cons)::void
      (let ((context (recons index context)))
        (with-translation screen (left top)
          (if (null? (head pair))
              (draw-empty-list! (null-head-space
				 pair))
              (draw! (head pair)
                     screen: screen
                     cursor: cursor
                     context: context
		     anchor: anchor)))))

    (define (draw-dotted-tail! pair::cons)::void
      (cond ((should-the-bar-be-horizontal? pair)
	     (with-translation screen (0 top)
	       (screen:draw-horizontal-bar!
				max-width))
             (skip-spaces! (skip-first-line
			    (pre-tail-space pair)))
             (let ((context (recons index context)))
	       (with-translation screen (left top)
		 (if (null? (tail pair))
		     (draw-empty-list!
		      (null-tail-space
		       pair))
		     (draw! (tail pair)
			    screen: screen
			    cursor: cursor
			    context: context
			    anchor: anchor))))
             (advance! (tail-extent pair screen))
             (skip-spaces! (post-tail-space pair)))
            (else
	     (with-translation screen (left top)
	       (screen:draw-vertical-bar!
		max-line-height))
             (advance!
	      (Extent
	       width: (screen:vertical-bar-width)
	       height: 0))
             (skip-spaces! (pre-tail-space pair))
             (let ((context (recons index context)))
	       (with-translation screen (left top)
		 (if (null? (tail pair))
		     (draw-empty-list!
		      (null-tail-space pair))
		     (draw! (tail pair)
			    screen: screen
			    cursor: cursor
			    context: context
			    anchor: anchor))))
             (advance! (tail-extent pair screen))
             (skip-spaces! (post-tail-space pair))
             )))

    (define (draw-next! pair::cons)::void
      (draw-head! pair)
      (advance! (head-extent pair screen))
      (skip-spaces! (post-head-space pair))
      (cond ((dotted? pair)
             (draw-dotted-tail! pair))

            ((pair? (tail pair))
             (draw-next! (tail pair)))))

    (skip-spaces! (pre-head-space elems))
    (draw-next! elems)
    ))

(define (cursor-under left::real top::real
		      elems
		      #!key
		      (screen::Screen
		       (current-screen))
		      (context::Cursor '()))
  ::Cursor
  (let ((box (extent elems screen))
	(max-width 0)
	(max-line-height (screen:min-line-height))
	(side (screen:paren-width))
	(ceiling 0)
	(index (first-index elems)))

    (define (check-spaces! space::Space)::Cursor
      (set! index (next-index index elems))
      #!null
      #;(let loop ((i 0))
	(cond ((is i >= (string-length spaces))
	       (set! index (next-index index elems))
	       #!null)
	      
	      ((eq? (spaces i) #\newline)
	       (set! ceiling
		 (+ ceiling max-line-height))
	       (set! side (screen:paren-width))
	       (set! max-line-height
		 (screen:min-line-height))
	       (cond ((is top < ceiling)
	              (recons* i index context))
		     (else
		      (loop (+ i 1)))))
	      
	      (else
	       (set! side (+ side 1))
	       (set! max-width (max max-width side))
	       (cond
		((and (is side < left <= (+ side 1))
		      (is top <= (+ max-line-height
				    ceiling)))
		 (recons* i index context))
		(else
		 (loop (+ i 1))))))))

    (define (advance! extent::Extent)::Null
      (set! side (+ side extent:width))
      (set! max-line-height (max extent:height
				 max-line-height))
      (set! max-width (max side max-width))
      #!null)

    (define (check! part extent::Extent)::Cursor
      (if (and (is side <= left <= (+ side
				      extent:width))
	       (is ceiling
		   <=
		   top
		   <=
		   (+ ceiling
		      extent:height)))
	  (let ((cursor (recons index context)))
	    (or (and (pair? part)
		     (cursor-under (- left side)
				   (- top ceiling)
				   part
				   screen: screen
				   context: cursor))
		cursor))
	  (advance! extent)))

    (define (check-separating-bar! pair)::Cursor
      (cond ((should-the-bar-be-horizontal? pair)
	     (let ((bar-height
		    (screen:horizontal-bar-height)))
	       (if (is ceiling
		       <=
		       top
		       <=
		       (+ ceiling
			  bar-height))
		   (recons index context)
		   (advance!
		    (Extent
		     width: 0
		     height: bar-height)))))
	    (else
	     (let ((bar-width
		    (screen:vertical-bar-width)))
	       (if (is side <= left (+ side
				       bar-width))
		   (recons index context)
		   (advance! (Extent
			      width: bar-width
			      height: 0)))))))

    (define (check-next! pair)::Cursor
      (or (check! (head pair) (head-extent pair
					   screen))
	  (check-spaces! (post-head-space pair))
	  (and (dotted? pair)
	       (or (check-separating-bar! pair)
		   (check-spaces! (pre-tail-space
				   pair))
		   (check! (tail pair)
			   (tail-extent pair
					screen))
		   (check-spaces! (post-tail-space
				   pair))))
	  (and (pair? (tail pair))
	       (check-next! (tail pair)))))

    (or (and (is 0 <= left < (screen:paren-width))
	     (is 0 <= top < box:height)
	     (recons index context))
	(and (is (- box:width
		    (screen:paren-width))
		 <=
		 left
		 <=
		 box:width)
	     (is 0 <= top < box:height)
	     (recons (last-index elems) context))

	(check-spaces! (pre-head-space elems))
	(check-next! elems)
	#!null)))

(define (sequence-extent elems screen::Screen)
  ::Extent
  (let ((max-width 0)
        (max-line-height (screen:min-line-height))
        (top 0)
        (left 0))

    (define (skip-spaces! space::Space)::void
      (match space:fragments
	(`(,first ,second . ,rest)
	 (set! max-width
	   (fold-left max (max max-width
			       (+ left first)
			       second)
		      rest))
	 (set! left
	   (if (null? rest)
	       second
	       (last rest)))
	 (set! top
	   (+ top
	      max-line-height
	      (* (screen:min-line-height)
		 (length rest))))
	 (set! max-line-height
	   (screen:min-line-height)))
	(`(,single)
	 (set! left (+ left single))
	 (set! max-width
	   (max max-width left)))))

    (define (advance! extent::Extent)::void
      (set! left (+ left extent:width))
      (set! max-line-height (max extent:height
				 max-line-height))
      (set! max-width (max left max-width)))

    (define (dotted-tail-extent pair::cons)::Extent
      (cond ((should-the-bar-be-horizontal? pair)
	     (skip-spaces! (skip-first-line
			    (pre-tail-space pair)))
	     (advance! (tail-extent pair screen))
	     (skip-spaces! (post-tail-space pair)))
	    (else
	     (advance!
	      (Extent
	       width: (screen:vertical-bar-width)
	       height: 0))
	     (skip-spaces! (pre-tail-space pair))
	     (advance! (tail-extent pair screen))
	     (skip-spaces! (post-tail-space pair))))
      (Extent width: max-width
              height: (+ top max-line-height)))

    (define (grow-ahead! pair)
      (advance! (head-extent pair screen))
      (skip-spaces! (post-head-space pair))
      (cond ((dotted? pair)
             (dotted-tail-extent pair))

            ((pair? (tail pair))
             (grow-ahead! (tail pair)))

            (else
             (Extent width: max-width
                     height: (+ top
				max-line-height)))))

    (skip-spaces! (pre-head-space pair))
    (grow-ahead! elems)
    ))


;; RZM37UHSPY5Z

