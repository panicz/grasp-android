(import (define-type))
(import (define-interface))
(import (define-property))
(import (define-object))
(import (define-cache))
(import (fundamental))
(import (indexable))
(import (infix))
(import (examples))
(import (match))
(import (for))
(import (functions))
(import (assert))
(import (print))
(import (conversions))
(import (srfi :11))
(import (painter))
(import (traversal))
(import (cursor))
(import (extent))

(define (fragment-size fragment)
  (match fragment
    (`(line-comment . ,text)
     0)
    
    (`(expression-comment ,space . ,expressions)
     (length expressions))

    (`(block-comment . ,text)
     0)
    
    (,@integer?
     fragment)))

(define (space-fragment-index fragments index)
  (if (or (isnt fragments pair?)
	  (is (fragment-size (head fragments)) >= index))
      (values fragments index)
      (space-fragment-index (tail fragments)
			    (- index
			       (fragment-size
				(head fragments))
			       1))))

(e.g.
 (space-fragment-index '(0 0) 0)
 ===> (0 0) 0)

(e.g.
 (space-fragment-index '(0 0) 1)
 ===> (0) 0)

(e.g.
 (space-fragment-index '(1 0) 0)
 ===> (1 0) 0)

(e.g.
 (space-fragment-index '(1 0) 1)
 ===> (1 0) 1)

(e.g.
 (space-fragment-index '(1 0) 2)
 ===> (0) 0)

(e.g.
 (space-fragment-index '(3 5) 0)
 ===> (3 5) 0)

(e.g.
 (space-fragment-index '(3 5) 1)
 ===> (3 5) 1)

(e.g.
 (space-fragment-index '(3 5) 2)
 ===> (3 5) 2)

(e.g.
 (space-fragment-index '(3 5) 3)
 ===> (3 5) 3)

(e.g.
 (space-fragment-index '(3 5) 4)
 ===> (5) 0)

(e.g.
 (space-fragment-index '(3 5) 5)
 ===> (5) 1)

(e.g.
 (space-fragment-index '(3 5) 6)
 ===> (5) 2)

(e.g.
 (space-fragment-index '(3 5) 7)
 ===> (5) 3)

(e.g.
 (space-fragment-index '(3 5) 8)
 ===> (5) 4)

(e.g.
 (space-fragment-index '(3 5) 9)
 ===> (5) 5)

(e.g.
 (space-fragment-index '(3 5) 10)
 ===> () 0)


(define (delete-space-fragment! fragments::pair
				position::int)
  ::pair
  (let-values (((cell index) (space-fragment-index
			      fragments
			      position)))
    (match cell
      (`(,,index ,next . ,rest)
       (set! (head cell) (+ index next))
       (set! (tail cell) rest)
       fragments)
      (`(,,@(is _ > 0) . ,_)
       (set! (head cell) (- (head cell) 1))
       fragments)
      (_
       fragments
       ))))
    

(e.g.
 (let* ((fragments (list 1 2 3))
	(result (delete-space-fragment! fragments 0)))
   (and (equal? result '(0 2 3))
	(eq? result fragments))))

(e.g.
 (let* ((fragments (list 1 2 3))
	(result (delete-space-fragment! fragments 1)))
   (and (equal? result '(3 3))
	(eq? result fragments))))

(e.g.
 (let* ((fragments (list 1 2 3))
	(result (delete-space-fragment! fragments 2)))
   (and (equal? result '(1 1 3))
	(eq? result fragments))))

(e.g.
 (let* ((fragments (list 1 2 3))
	(result (delete-space-fragment! fragments 3)))
   (and (equal? result '(1 1 3))
	(eq? result fragments))))

(e.g.
 (let* ((fragments (list 1 2 3))
	(result (delete-space-fragment! fragments 4)))
   (and (equal? result '(1 5))
	(eq? result fragments))))

(e.g.
 (let* ((fragments (list 1 2 3))
	(result (delete-space-fragment! fragments 5)))
   (and (equal? result '(1 2 2))
	(eq? result fragments))))

(e.g.
 (let* ((fragments (list 1 2 3))
	(result (delete-space-fragment! fragments 6)))
   (and (equal? result '(1 2 2))
	(eq? result fragments))))

(e.g.
 (let* ((fragments (list 1 2 3))
	(result (delete-space-fragment! fragments 7)))
   (and (equal? result '(1 2 2))
	(eq? result fragments))))

(define-type (Space fragments: pair)
  implementing Element with
  ((part-at index::Index)::Indexable*
   (let-values (((fragments* index*) (space-fragment-index
				      fragments index)))
     (if (or (isnt fragments* pair?)
	     (number? (head fragments*)))
	 (this)
	 (match (head fragments*)
	   (`(line-comment . ,text)
	    (assert (= index* 0))
	    text)
	   
	   (`(expression-comment ,space . ,expressions)
	    (if (= index* 0)
		space
		(list-ref expressions (- index* 1))))

	   (`(block-comment . ,text)
	    (assert (= index* 0))
	    text)))))

  ((first-index)::Index 0)
  
  ((last-index)::Index
   (fold-left (lambda (total next)
		(if (number? next)
		    (+ total next)
		    (match next
		      (`(line-comment . ,text)
		       0)
		      
		      (`(expression-comment ,space
					    . ,expressions)
		       (length expressions))

		      (`(block-comment . ,text)
		       0))))

		(length (tail fragments))
	      fragments))
  
  ((next-index index::Index)::Index
   (min (+ index 1) (last-index)))
  
  ((previous-index index::Index)::Index
   (max (first-index) (- index 1)))
   
  ((index< a::Index b::Index)::boolean
   (is (as int a) < (as int b)))

  ((delete-space! position::int)::void
   (delete-space-fragment! fragments position))
  
  ((insert-space! position::int)::void
   (let-values (((cell index) (space-fragment-index
			       fragments
			       position)))
     (set! (head cell) (+ (head cell) 1))))

  ((insert-break! position::int)::void
   (let-values (((cell index) (space-fragment-index
			       fragments
			       position)))
     (set! (tail cell) (cons 0 (tail cell)))))

  ((draw! context::Cursor)::void
   (let* ((painter (the-painter))
	  (space-width (painter:space-width))
	  (t (invoke (the-traversal) 'clone)))
     (let skip ((input fragments)
		(total 0))
       (define (advance-with-cursor! width::real)
	 (let ((width (* width space-width)))
	   (and-let* ((`(,tip ,next . ,sub) (the-cursor))
		      ((integer? tip))
		      ((equal? sub context))
		      ((eqv? next t:index))
		      ((is total <= tip <= (+ total
					      width))))
	     (painter:remember-offset!
	      (+ t:left (- tip total)) (+ t:top 2)))
	   (t:advance-by! width)))
       
       (match input
	 (`(,,@integer? ,,@integer? . ,_)
	  (advance-with-cursor! (head input))
	  (t:new-line!)
	  (skip (tail input) (+ total (head input) 1)))
	 (`(,,@integer)
	  (advance-with-cursor! (head input)))))))

  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   (and-let* ((painter (the-painter))
	      (space-width (painter:space-width))
	      (tentative-result #f)
	      (t (invoke (the-traversal) 'clone))
	      ((is y < t:top)))
     (let skip ((input fragments)
		(total 0))
       (match input
	 (`(,,@integer? ,,@integer? . ,rest)
	  (cond
	   ((is t:top <= y < (+ t:top t:max-line-height))
	    (hash-cons* (+ total
			   (min (head input)
				(quotient (- x t:left)
					  space-width)))
			t:index
			path))
	   (else
	    (t:advance-by! (* space-width (head input)))
	    (t:new-line!)
	    (skip (tail input)
		  (+ total (head input))))))
	 (`(,,@integer?)
	  (and (is t:left <= x < (+ t:left
				 (* space-width
				    (head input))))
	       (hash-cons* (+ total
			      (quotient (- x t:left)
					space-width))
			   t:index
			   path)))))))
  
  ((print out::gnu.lists.Consumer)::void
   (let process ((input fragments))
     (match input
       (`(,,@integer? ,,@integer? . ,_)
	(for n from 0 below (head input)
	     (out:append #\space))
	(out:append #\newline)
	(process (tail input)))

       (`(,,@integer? . ,rest)
	(for n from 0 below (head input)
	     (out:append #\space))
	(process rest))

       (`((line-comment . ,line-comment) . ,rest)
	(out:append #\;)
	(for c in line-comment
	  (out:append (as char c)))
	(out:append #\newline)
	(process rest))
       
       (`((expression-comment
	   ,spaces . ,expressions) . ,rest)
	;; `expressions' should always contain
	;; exactly one element
	(out:append #\#)
	(out:append #\;)
	(invoke (as Space spaces) 'print out)
	(for expression in expressions
	     (show expression))
	(process rest))

       (`((block-comment . ,comment) . ,rest)
	(out:append #\#)
	(out:append #\|)
	(for c::gnu.text.Char in comment
	     (out:append c))
	(out:append #\|)
	(out:append #\#)
	(process rest))

       (_
	(values)))))

  ((advance! t::Traversal)::void
   (let* ((painter (the-painter))
	  (space-width (painter:space-width)))
     (let skip ((input fragments)
		(total 0))
       (match input
	 (`(,,@integer? ,,@integer? . ,_)
	  (t:advance-by! (* space-width (head input)))
	  (t:new-line!)
	  (skip (tail input) (+ total (head input) 1)))
	 (`(,,@integer?)
	  (t:advance-by! (* space-width (head input))))))
     (set! t:index (+ t:index 1))))
  )


(define (insert-space! space::Space position::int)
  (space:insert-space! position))

(define (insert-break! space::Space position::int)
  (space:insert-break! position))

(define (insert-whitespace! c::char space::Space
			    position::int)
  (assert (char-whitespace? c))
  (if (eq? c #\newline)
      (insert-break! space position)
      (insert-space! space position)))

(define (delete-space! space::Space position::int)
  (invoke space 'delete-space! position))

(define (join-spaces! a::Space b::Space)::Space
  (let ((suffix (last-pair a:fragments)))
    (set! (head suffix)
      (+ (head suffix) (head b:fragments)))
    (set! (tail suffix) (tail b:fragments))
    (set! b:fragments (cons 0 '()))
    a))

(define (split-space! space::Space index::int)::Space
  "Truncates space and returns the rest in the result"
  (define (split-fragments! fragments::pair
			    index::int)
    ::Space
    (cond
     ((is index <= (head fragments))
      (let ((reminent (cons (- (head fragments) index)
			    (tail fragments))))
	(set! (head fragments) index)
	(set! (tail fragments) '())
	(Space fragments: reminent)))
     (else
      (split-fragments!
       (tail fragments)
       (- index (head fragments) 1)))))
  (split-fragments! space:fragments index))

(e.g.
 (let* ((fragments (list 3 6 9))
	(space (Space fragments: fragments))
	(rest (split-space! space 0)))
   (and (equal? space (Space fragments: '(0)))
	(equal? rest (Space fragments: '(3 6 9))))))

(e.g.
 (let* ((space (Space fragments: (list 3 6 9)))
	(rest (split-space! space 1)))
   (and (equal? space (Space fragments: '(1)))
	(equal? rest (Space fragments: '(2 6 9))))))

(e.g.
 (let* ((space (Space fragments: (list 3 6 9)))
	(rest (split-space! space 3)))
   (and (equal? space (Space fragments: '(3)))
	(equal? rest (Space fragments: '(0 6 9))))))

(e.g.
 (let* ((space (Space fragments: (list 3 6 9)))
	(rest (split-space! space 4)))
   (and (equal? space (Space fragments: '(3 0)))
	(equal? rest (Space fragments: '(6 9))))))

(e.g.
 (let* ((space (Space fragments: (list 3 6 9)))
	(rest (split-space! space 5)))
   (and (equal? space (Space fragments: '(3 1)))
	(equal? rest (Space fragments: '(5 9))))))

(e.g.
 (let* ((space (Space fragments: (list 3 6 9)))
	(rest (split-space! space 10)))
   (and (equal? space (Space fragments: '(3 6)))
	(equal? rest (Space fragments: '(0 9))))))

(e.g.
 (let* ((space (Space fragments: (list 3 6 9)))
	(rest (split-space! space 11)))
   (and (equal? space (Space fragments: '(3 6 0)))
	(equal? rest (Space fragments: '(9))))))

(define (skip-first-line s::Space)::Space
  (match s:fragments
    (`(,_ ,_ . ,_)
     (Space fragments: (tail s:fragments)))
    (_
     (Space fragments: '(0)))))

;; a cell is "dotted?" naturally when it is
;; a pair whose "tail" isn't a list (so for example,
;; if it's a symbol or a number).

;; But every cell can be stipulated to be "dotted?".
;; However, it is an error (currently unhandled)
;; to stipulate a cell whose tail isn't a list
;; to be not dotted, i.e. it is an error to invoke
;;
;; (set! (dotted? pair) #f)
;;
;; Instead, (unset! (dotted? pair)) should be used.

(define-property (dotted? cell)
  (not (or (null? (tail cell))
	   (pair? (tail cell)))))

;; `pre-head-space` appears before the first element
;; of a list, after the opening paren (therefore
;; it should be considered sparse).

(define-property+ (pre-head-space cell)
  (Space fragments: (cons 0 '())))

;; `post-head-space` appears after each element
;; in a list (and should therefore be considered
;; dense: expect as many `post-head-space`s as there
;; are cells visible in the document.

(define-property+ (post-head-space cell)
  (if (and (not (dotted? cell))
	   (null? (tail cell)))
      (Space fragments: (cons 0 '()))
      (Space fragments: (cons 1 '()))))

;; `pre-` and `post-tail-space` only appear
;; in the pairs that are `dotted?` (so they
;; can both be considered sparse)

(define-property+ (pre-tail-space cell)
  (Space fragments: (cons 1 '())))

(define-property+ (post-tail-space cell)
  (Space fragments: (cons 0 '())))

;; `null-head-space` only concerns a situation
;; where the head of a list is `null?`.
;; Since all empty lists are considered the same
;; in Lisp, the only way to set the size
;; of a particular 

(define-property+ (null-head-space cell)
  (Space fragments: (cons 0 '())))

;; the `null-tail-space` property only concerns
;; a situation when a cell is stipulated to be
;; `dotted?` and its tail is `null?`.

(define-property+ (null-tail-space cell)
  (Space fragments: (cons 0 '())))

(define-object (HeadTailSeparator)::Indexable
  (define (part-at index::Index)::Indexable* (this))
  (define (first-index)::Index #\|)
  (define (last-index)::Index #\|)
  (define (next-index index::Index)::Index #\|)
  (define (previous-index index::Index)::Index #\|)
  (define (index< a::Index b::Index) #f)
  
  (define (toString)::String "|"))

(define-constant head/tail-separator
  (HeadTailSeparator))

(define (head/tail-separator? x)
  (instance? x HeadTailSeparator))

(define-object (HorizontalBar width0::real)::Tile
  (define width :: real 0)
  (define (draw! context::Cursor)::void
   (invoke (the-painter) 'draw-horizontal-bar! width))
  (define (extent)::Extent
    (Extent width: width
	    height: (invoke (the-painter) 'horizontal-bar-height)))
  
  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (let ((inner (extent)))
      (and (is 0 <= x < inner:width)
	   (is 0 <= y < inner:height)
	   (hash-cons (invoke (this) 'first-index) path))))

  (HeadTailSeparator)
  (set! width width0))

(define-object (VerticalBar height0::real)::Tile
  (define height :: real 0)
  (define (draw! context::Cursor)::void
    (invoke (the-painter) 'draw-vertical-bar! height))
  
  (define (extent)::Extent
    (Extent width: (invoke (the-painter) 'vertical-bar-width)
	    height: height))
  
  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (let ((inner (extent)))
      (and (is 0 <= x < inner:width)
	   (is 0 <= y < inner:height)
	   (hash-cons (invoke (this) 'first-index) path))))

  (HeadTailSeparator)
  (set! height height0))

(define-cache (horizontal-bar width)
  (HorizontalBar (as real width)))

(define-cache (vertical-bar height)
  (VerticalBar (as real height)))

(define (should-the-bar-be-horizontal? dotted-pair)
  ::boolean
  (assert (dotted? dotted-pair))
  (and-let* (((Space fragments: `(,_ ,_ . ,_))
	      (post-head-space dotted-pair))
	     ((Space fragments: `(,_ ,_ . ,_))
	      (pre-tail-space dotted-pair)))))

(define-property (head-tail-separator cell)
  head/tail-separator)

(define-object (EmptyListProxy inner::Space)::Tile  
  (define space :: Space)
  
  (define (part-at index::Index)::Indexable*
    (match index
      (0 space)
      (_ (this))))
  
  (define (first-index)::Index #\[)
  
  (define (last-index)::Index #\])
  
  (define (next-index index::Index)::Index
    (match index
      (#\[ 0)
      (_ #\])))
  
  (define (previous-index index::Index)::Index
    (match index
      (#\] 0)
      (_ #\[)))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (let* ((outer (extent))
	   (painter (the-painter))
	   (paren-width (painter:paren-width)))
      (and (is 0 <= x < outer:width)
	   (is 0 <= y < outer:height)
	   (cond ((is 0 <= x < paren-width)
		  (hash-cons (first-index) path))
		 ((is paren-width <= x < (- outer:width
					    paren-width))
		  (hash-cons 0 path))
		 ((is (- outer:width paren-width) <= x)
		  (hash-cons (last-index) path))))))
  
  (define (index< a::Index b::Index)
    (or (and (eqv? a #\[) (or (eqv? b 0)
			      (eqv? b #\])))
	(and (eqv? a 0) (eqv? b #\]))))

  (define (extent)::Extent
    (let ((traversal (Traversal))
	  (painter (the-painter)))
      (space:advance! traversal)
      (Extent width: (+ (* 2 (painter:paren-width))
			traversal:max-width)
	      height: (+ traversal:top traversal:max-line-height))))

  (define (draw! context::Cursor)::void
    (let ((outer (extent))
	  (painter (the-painter)))
      (painter:open-paren! outer:height)
      (with-translation ((- outer:width (painter:paren-width)) 0)
	  (painter:close-paren! outer:height))))
  
  (gnu.lists.LList)
  (set! space inner))

(define-cache (empty-list-proxy space)
  (EmptyListProxy (as Space space)))

(define cell-display-properties
  (list
   dotted?
   pre-head-space
   post-head-space
   pre-tail-space
   post-tail-space
   null-head-space
   null-tail-space))

(define (copy-properties properties original cell)
  (for property in properties
    (update! (property cell) (property original)))
  cell)

(define (tail-space-to-head original cell)
  (update! (pre-head-space cell)
	   (pre-tail-space original))
  (update! (post-head-space cell)
	   (post-tail-space original))
  (update! (null-head-space cell)
	   (null-tail-space original))
  cell)

(define (head-space-to-tail original cell)
  (update! (pre-tail-space cell)
	   (pre-head-space original))
  (update! (post-tail-space cell)
	   (post-head-space original))
  (update! (null-tail-space cell)
	   (null-head-space original))
  cell)

(define (tree-map/preserve properties f l)
  (if (pair? l)
      (copy-properties
       properties
       l
       (cons
	(tree-map/preserve properties f (head l))
	(tree-map/preserve properties f (tail l))))
      (f l)))

(define (null-space-ref grandparent::pair
			parent-index::int)
  (match parent-index
    (0 (pre-head-space grandparent))
    (1 (null-head-space grandparent))
    (2 (post-head-space grandparent))
    (_
     (if (dotted? grandparent)
	 (match parent-index
	   (3 (head-tail-separator grandparent))
	   (4 (pre-tail-space grandparent))
	   (5 (null-tail-space grandparent))
	   (6 (post-tail-space grandparent)))
	 (null-space-ref (tail grandparent)
			 (- parent-index 2))))))

(define (print-space space::Space
		     #!optional (port (current-output-port)))
  #;(write space:fragments port)
  (space:print port))

(define (show-empty-list space)::void
  (write-char #\()
  (print-space space)
  (write-char #\)))

(define (show-head p::pair)::void
  (if (null? (head p))
      (show-empty-list (null-head-space p))
      (show (head p))))

(define (show-dotted-tail p::pair)::void
  (write-char #\.)
  (print-space (pre-tail-space p))
  (if (null? (tail p))
      (show-empty-list (null-tail-space p))
      (show (tail p)))
  (print-space (post-tail-space p)))

(define (show-pair p::pair)::void
  (show-head p)
  (print-space (post-head-space p))
  (cond ((dotted? p)
	 (show-dotted-tail p))
	((pair? (tail p))
	 (show-pair (tail p)))))

(define (show p)::void
  (cond
   ((pair? p)
    (write-char #\()
    (print-space (pre-head-space p))
    (show-pair p)
    (write-char #\)))
   (else
    (write p))))

(define (show-document d::pair)
  (cond ((null? (head d))
	 (print-space (null-head-space d)))
	((pair? (head d))
	 (print-space (pre-head-space (head d)))
	 (show-pair (head d)))))

(define (show->string p)::string
  (with-output-to-string
    (lambda ()
      (show p))))
