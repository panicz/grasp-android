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
(import (space))
(import (cursor))
(import (tile))
(import (for))
(import (screen))
(import (functions))
(import (print))
(import (conversions))

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
;; see (and operate on) Atoms themselves
(define-constant evaluating?::parameter[boolean]
  (make-parameter #f))

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
    (invoke (the-screen) 'draw-atom! source
	    (and (pair? (the-cursor))
		 (equal? (cursor-tail) context)
		 (cursor-head))))

  (define (extent)::Extent
    (invoke (the-screen) 'atom-extent source))
  
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
    (Atom (invoke source 'substring start)))

  (define (toString)::String
    source)
  
  (set! builder (java.lang.StringBuilder source-string))
  (set! source (builder:toString)))

(define-object (cons a d)::Tile
  (define (equals object)::boolean
   (eq? object (this)))

  (define (hash-code)::int
    (java.lang.System:identity-hash-code (this)))

  (define (draw! context::Cursor)
    ::void
    (let ((inner (sequence-extent (this)))
	  (paren-width (invoke (the-screen) 'paren-width)))
      (invoke (the-screen) 'open-paren! inner:height 0 0)
      (when (equal? (the-cursor) (recons (first-index)
					 context))
	(invoke (the-screen) 'remember-offset! 0 2))
      (with-translation (paren-width 0)
	(draw-sequence! (this) context: context))
      (invoke (the-screen) 'close-paren! inner:height
	      (+ paren-width inner:width) 0)
      (when (equal? (the-cursor) (recons (last-index)
					 context))
	(invoke (the-screen) 'remember-offset!
		(+ paren-width 1 inner:width)
		(- inner:height 1)))
      ))

  (define (extent)::Extent
    (let ((extent ::Extent (sequence-extent
			    (this))))
      (Extent width: (+ extent:width
			(* 2 (invoke (the-screen)
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

(define-cache (heads tail)
  (cache (head)
	 (cons head tail)))

(define (recons head tail)::cons
  ((heads tail) head))

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
	  height: (* (invoke (the-screen) 'min-line-height)
		     (length space:fragments))))

(define (head-extent pair::cons)
  ::Extent
  (if (null? (head pair))
      (let ((inner (empty-space-extent
		    (null-head-space pair))))
	(Extent width: (+ inner:width
			  (* 2 (invoke (the-screen)
				       'paren-width)))
		height: inner:height))
      (extent (head pair))))

(define (tail-extent pair::cons)
  ::Extent
  (if (null? (tail pair))
      (let ((inner (empty-space-extent
		    (null-head-space pair))))
	(Extent width: (+ inner:width
			  (* 2 (invoke (the-screen)
				       'paren-width)))
		height: inner:height))
      (extent (tail pair))))

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

(define (draw-sequence! elems::list #!key
			(context::Cursor (recons 1 '())))
  ::void
  (let ((max-width 0)
        (max-line-height (invoke (the-screen)
				 'min-line-height))
        (top 0)
        (left 0)
        (index 0))

    (define (skip-spaces! space::Space)::void
      (let skip ((input space:fragments)
		 (total 0))
	(define (advance-with-cursor! width::real)
	  (and-let* ((`(,tip ,next . ,sub) (the-cursor))
		     ((integer? tip))
		     ((equal? sub context))
		     ((eqv? next index))
		     ((is total <= tip <= (+ total
					     width))))
	    (invoke (the-screen) 'remember-offset!
		    (+ left (- tip total)) (+ top 2)))
	  (set! left (+ left width))
	  (set! max-width (max max-width left)))
	
	(match input
	  (`(,,@integer? ,,@integer? . ,_)
	   (advance-with-cursor! (head input))
	   (set! top (+ top max-line-height))
           (set! left 0)
           (set! max-line-height
		 (invoke (the-screen) 'min-line-height))
	   (skip (tail input) (+ total (head input) 1)))
	  (`(,,@integer)
	   (advance-with-cursor! (head input)))))
      (set! index (+ index 1)))

    (define (advance! extent::Extent)::void
      (when (equal? (the-cursor) (recons index context))
	(invoke (the-screen) 'remember-offset! left (+ top 2)))
      (set! left (+ left extent:width))
      (set! max-line-height (max extent:height
				 max-line-height))
      (set! max-width (max left max-width))
      (set! index (+ index 1)))

    (define (draw-empty-list! space::Space context)::void
      (let ((inner (empty-space-extent space))
	    (paren-width (invoke (the-screen) 'paren-width)))
	(invoke (the-screen) 'open-paren! inner:height 0 0)
	(invoke (the-screen) 'close-paren! inner:height
		(+ paren-width inner:width) 0)
	(match (the-cursor)
	  (`(#\[ . ,,context)
	   (invoke (the-screen) 'remember-offset! 0 2))
	  (`(#\] . ,,context)
	   (invoke (the-screen) 'remember-offset!
		   (+ paren-width 1 inner:width)
		   (- inner:height 1)))
	  (`(,,@number? 0 . ,,context)
	   (invoke (the-screen) 'remember-offset!
		   (+ 1 (cursor-head)) 2))
	  (_ (values)))))
    
    (define (draw-head! pair::cons)::void
      (let ((context (recons index context)))
        (with-translation (left top)
          (if (null? (head pair))
              (draw-empty-list! (null-head-space
				 pair)
				context)
              (draw! (head pair) context: context)))))

    (define (draw-dotted-tail! pair::cons)::void
      (cond ((should-the-bar-be-horizontal? pair)
	     (with-translation (0 top)
		 (invoke (the-screen) 'draw-horizontal-bar!
			 max-width))
             (skip-spaces! (skip-first-line
			    (pre-tail-space pair)))
             (let ((context (recons index context)))
	       (with-translation (left top)
		 (if (null? (tail pair))
		     (draw-empty-list!
		      (null-tail-space
		       pair)
		      context)
		     (draw! (tail pair)
			    context: context))))
             (advance! (tail-extent pair))
             (skip-spaces! (post-tail-space pair)))
            (else
	     (with-translation (left top)
		 (invoke (the-screen) 'draw-vertical-bar!
			 max-line-height))
             (advance!
	      (Extent
	       width: (invoke (the-screen) 'vertical-bar-width)
	       height: 0))
             (skip-spaces! (pre-tail-space pair))
             (let ((context (recons index context)))
	       (with-translation (left top)
		 (if (null? (tail pair))
		     (draw-empty-list!
		      (null-tail-space pair)
		      context)
		     (draw! (tail pair)
			    context: context))))
             (advance! (tail-extent pair))
             (skip-spaces! (post-tail-space pair))
             )))

    (define (draw-next! pair::cons)::void
      (draw-head! pair)
      (advance! (head-extent pair))
      (skip-spaces! (post-head-space pair))
      (cond ((dotted? pair)
             (draw-dotted-tail! pair))

            ((pair? (tail pair))
             (draw-next! (tail pair)))))

    (unless (null? elems)
      (skip-spaces! (pre-head-space elems))
      (draw-next! elems))
    ))

(define (draw! object #!key
	       (context::Cursor '()))
  ::void
  (cond ((instance? object Tile)
	 (invoke (as Tile object)
		 'draw! context))

	((null? object)
	 (values))

	(else
	 (with-translation (0 1)
	     (invoke (the-screen)
		     'draw-string!
		     (with-output-to-string
		       (lambda () (write object)))
		     (and (pair? (the-cursor))
			  (equal? (cursor-tail) context)
			  (cursor-head)))))))

(define (cursor-under left::real top::real
		      elems
		      #!key
		      (context::Cursor (recons 1 '())))
  ::Cursor
  (let ((box (extent elems))
	(max-width 0)
	(max-line-height (invoke (the-screen) 'min-line-height))
	(side (invoke (the-screen) 'paren-width))
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
	       (set! side (invoke (the-screen) 'paren-width))
      (set! max-line-height
		 (invoke (the-screen) 'min-line-height))
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
				   context: cursor))
		cursor))
	  (advance! extent)))

    (define (check-separating-bar! pair)::Cursor
      (cond ((should-the-bar-be-horizontal? pair)
	     (let ((bar-height (invoke
				(the-screen)
				'horizontal-bar-height)))
	       (if (is ceiling <= top <= (+ ceiling
					    bar-height))
		   (recons index context)
		   (advance!
		    (Extent
		     width: 0
		     height: bar-height)))))
	    (else
	     (let ((bar-width (invoke
			       (the-screen)
			       'vertical-bar-width)))
	       (if (is side <= left (+ side
				       bar-width))
		   (recons index context)
		   (advance! (Extent
			      width: bar-width
			      height: 0)))))))

    (define (check-next! pair)::Cursor
      (or (check! (head pair) (head-extent pair))
	  (check-spaces! (post-head-space pair))
	  (and (dotted? pair)
	       (or (check-separating-bar! pair)
		   (check-spaces! (pre-tail-space
				   pair))
		   (check! (tail pair)
			   (tail-extent pair))
		   (check-spaces! (post-tail-space
				   pair))))
	  (and (pair? (tail pair))
	       (check-next! (tail pair)))))

    (or (and (is 0 <= left < (invoke (the-screen) 'paren-width))
	     (is 0 <= top < box:height)
	     (recons index context))
	(and (is (- box:width
		    (invoke (the-screen)
			    'paren-width)) <= left <= box:width)
	     (is 0 <= top < box:height)
	     (recons (last-index elems) context))

	(check-spaces! (pre-head-space elems))
	(check-next! elems)
	#!null)))

(define (sequence-extent elems)
  ::Extent
  (let ((max-width 0)
        (max-line-height (invoke (the-screen) 'min-line-height))
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
	      (* (invoke (the-screen) 'min-line-height)
		 (length rest))))
	 (set! max-line-height
	   (invoke (the-screen) 'min-line-height)))
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
	     (advance! (tail-extent pair))
	     (skip-spaces! (post-tail-space pair)))
	    (else
	     (advance!
	      (Extent
	       width: (invoke (the-screen) 'vertical-bar-width)
	       height: 0))
	     (skip-spaces! (pre-tail-space pair))
	     (advance! (tail-extent pair))
	     (skip-spaces! (post-tail-space pair))))
      (Extent width: max-width
              height: (+ top max-line-height)))

    (define (grow-ahead! pair)
      (advance! (head-extent pair))
      (skip-spaces! (post-head-space pair))
      (cond ((dotted? pair)
             (dotted-tail-extent pair))

            ((pair? (tail pair))
             (grow-ahead! (tail pair)))

            (else
             (Extent width: max-width
                     height: (+ top
				max-line-height)))))
    (skip-spaces! (pre-head-space elems))
    (grow-ahead! elems)
    ))


(define (extent object)
  ::Extent
  (cond ((instance? object Tile)
	 (invoke (as Tile object) 'extent))

	((null? object)
	 (Extent width: 0 height: (invoke (the-screen)
					  'min-line-height)))

	((pair? object)
	 (sequence-extent object))

	((symbol? object)
	 (invoke (the-screen) 'atom-extent
		 (symbol->string object)))
	
	(else
	 (invoke (the-screen) 'atom-extent
		 (with-output-to-string
		   (lambda () (write object)))))
	))



