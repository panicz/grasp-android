(import (define-interface))
(import (define-type))
(import (define-property))
(import (match))
(import (infix))
(import (assert))
(import (for))
(import (examples))
(import (define-cache))
(import (print))
(import (string-building))
(import (functions))

#|

A Cursor is a list of things that can be used for
indexing tiles. Those can be any objects that can
be compared using the 'eqv?' predicate (except
#!null), but in practice those values can either
be integers, symbols orcharacters.

The order of elements in the cursor list is such 
that the first element of the list is an index of
the innermost element relative to its parent (which
is pointed to by the second element of the list, or
is the currently considered node if there isn't one)

This order doesn't allow to select the expression
pointed to by a cursor in a tail-recursive manner:
we need to reach the last index in order to choose
a sub-expression on a top-level.
The reason we chose this "reverse" order has to do
with the way we build those indices: we start from
the top level, and we descend deeper recursively;
therefore, we "cons" the inermost expressions' 
indices last.

Also, this strategy maximizes "structural sharing"
between cursors to different expressions
(which I think is beautiful), and reverting this
order would be wasteful; more specifically,
tail recursion wouldn't be much of a win here,
because the depth of expressions in a document
is going to be bounded anyway, and having
the boundedness of documents correspond to the
boundedness of the stack seems ok.

Another thing with cursors is that, when refering to
normal boxes (or "lists"), even indices usually
refer to spaces, and odd indices refer to 
subsequent elements in a list.

The exception is in the case of a dotted tail:
the odd index refers to the tail itself, as if 
it was an element, and the next odd index refers 
to the tail of the list.

Also, if the index is #\(, then it refers to the 
opening parentehsis of a box, and if it is #\), 
it refers to its closing parenthesis.

Every kind of tile manages its own cursor values,
so the source of every cursor value is a tile, which
controls the indices.
|#

(define-alias Cursor java.lang.Object)
;;gnu.lists.LList)

#|
Each tile can choose whatever it pleases to be 
its index except #!null, for the reason explained
below)

For built-in types (boxes, combinators, atoms) 
indices are typically either integers or characters
or symbols.

The special value #!null means the absence 
of an index
|#

(define-alias Index java.lang.Object)

(define-alias Indexable* java.lang.Object)

;; we consider the Null class to be a class whose
;; only member #!null
(define-alias Null java.lang.Object)

(define-type (Space fragments: pair)
  implementing StringBuilding
  with
  ((buildString out::StringBuilder)::StringBuilder
   (let process ((input fragments))
     (match input
       (`(,first . ,rest)
	(for n from 0 below first
	     (out:append #\space))
	(match rest
	  ('()
	   out)
	  (`(,_ . ,_)
	   (process rest)))))))
  ((toString)::String
   (invoke (buildString (StringBuilder)) 'toString))
  )

(define head car)

(define tail cdr)


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

(define-property+ (pre-head-space cell) "")

;; `post-head-space` appears after each element
;; in a list (and should therefore be considered
;; dense: expect as many `post-head-space`s as there
;; are cells visible in the document.

(define-property+ (post-head-space cell)
  (if (and (not (dotted? cell))
	   (null? (tail cell)))
      ""
      " "))

;; `pre-` and `post-tail-space` only appear
;; in the pairs that are `dotted?` (so they
;; can both be considered sparse)

(define-property+ (pre-tail-space cell) " ")

(define-property+ (post-tail-space cell) "")

;; `null-tail-head` only concerns a situation
;; where the head of a list is `null?`.
;; Since all empty lists are considered the same
;; in Lisp, the only way to set the size
;; of a particular 

(define-property+ (null-head-space cell) "")

;; the `null-tail-space` property only concerns
;; a situation when a cell is stipulated to be
;; `dotted?` and its tail is `null?`.

(define-property+ (null-tail-space cell) "")

(define-simple-class HeadTailSeparator ()
  ((toString)::String "."))

(define-constant head/tail-separator
  (HeadTailSeparator))

(define (head/tail-separator? x)
  (instance? x HeadTailSeparator))

(define-property (head-tail-separator cell)
  head/tail-separator)

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

(define-constant final-part?::parameter[boolean]
  (make-parameter #f))

(define (cell-index cell::pair index::int)
  (assert (is index >= 0))
  (cond ((= index 0)
         (pre-head-space cell)) ;; trzeba jakos rzutowac do Indexable
        ((= index 1)
         (let ((target (car cell)))
           (if (and (null? target)
		    (not (final-part?)))
               (null-head-space cell)
               target)))
        ((= index 2)
         (post-head-space cell)) ;; jak wyzej
        ((dotted? cell)
         (cond ((= index 3)
                (head-tail-separator cell))
               ((= index 4)
                (pre-tail-space cell)) ;; jakos rzutowac do Indexable
               ((= index 5)
                (let ((target (cdr cell)))
                  (if (and (null? target)
			   (not (final-part?)))
                      (null-tail-space cell)
                      target)))
               ((= index 6)
                (post-tail-space cell))))
        (else
         (cell-index (cdr cell) (- index 2)))))

(define (last-cell-index cell::pair
			 #!optional
			 (initial::int 2))
  ::int
  (cond ((dotted? cell)
         (+ initial 4))
        ((pair? (tail cell))
         (last-cell-index (tail cell)
			  (+ initial 2)))
        (else
         initial)))

(define-interface Indexable ()
  (has-children?)::boolean
  
  (part-at index::Index)::Indexable*
  
  (first-index)::Index
  (last-index)::Index
  
  (next-index index::Index)::Index
  (previous-index index::Index)::Index

  ;;(take-part-at! cursor::Cursor)::Indexable*
)

(define (has-children? object)
  (cond ((Indexable? object)
	 (invoke (as Indexable object)
		 'has-children?))

	((pair? object)
	 #t)

	((string? object)
	 #t)
	
	(else
	 #f)))

(define (part-at index::Index object)::Indexable*
  (cond ((Indexable? object)
	 (invoke (as Indexable object)
		 'part-at index))

	((pair? object)
	 (if (or (eq? index #\() (eq? index #\)))
	     object
	     (cell-index object (as int index))))
	 
	(else
	 object
	 #;(error "Don't know how to extract "index
		" from "object))))

(define (cursor-ref tile cursor::Cursor)
  (cond ((null? cursor)
         tile)
        ((pair? cursor)
         (let ((parent (cursor-ref tile (tail
					 cursor))))
           (if parent
	       (parameterize ((final-part?
			       (null? (tail
				       cursor))))
		 (part-at (head cursor) parent))
               parent)))
        (else
         (error "Unable to refer to cursor "cursor
		" in "tile))))

(define (first-index object)
  (cond ((Indexable? object)
	 (invoke (as Indexable object)
		 'first-index))

	((string? object)
	 0)

	((pair? object)
	 #\()
	
	(else
	 (error "Don't know how to obtain first index from "object))))

(define (last-index object)
  (cond ((Indexable? object)
	 (invoke (as Indexable object) 'last-index))

	((string? object)
	 (string-length (as string object)))

	((pair? object)
	 #\))
	
	(else
	 (error "Don't know how to obtain last index from "object))))

(define (next-index index::Index object)::Index
  (cond ((Indexable? object)
	 (invoke (as Indexable object)
		 'next-index index))

	((string? object)
	 (min (last-index object) (+ index 1)))

	((pair? object)
	 (match index
	   (#\( 0)
	   (#\) #\))
	   (,@(is _ < (last-cell-index object))
	    (+ index 1))
	   (_
	    #\))))
	
	(else
	 (error "Don't know how to obtain next index to "index
		" in "object))))

(define (previous-index index::Index object)::Index
  (cond ((Indexable? object)
	 (invoke (as Indexable object)
		 'previous-index index))

	((string? object)
	 (max 0 (- index 1)))

	((pair? object)
	 (match index
	   (0 #\()
	   (#\) (last-cell-index object))
	   (#\( #\()
	   (_ (- index 1))))
	
	(else
	 (error "Don't know how to obtain previous index to "index
		" in "object))))

(define (base-cursor cursor object)
  (match cursor
    (`(,index . ,context)
     (let* ((parent (cursor-ref object cursor))
	    (target (part-at index parent)))
       (if (eq? parent target)
	   context
	   cursor)))
    (_
     cursor)))


(define (subcursor cursor::Cursor context::Cursor)
  ::Cursor
  (and cursor
       (is context suffix? cursor)
       cursor))

#|
inny pomysl na kursor jest taki, ze to ciag 
dowolnych obiektow, czyli np. kombinatory moga
miec takie "indeksy", jak 'left czy 'right, ktore 
beda ze soba porownywanw za pomoca predykatu eqv?.

W kazdym razie jest tutaj jeszcze inny pomysl:
zeby zamiast stringa ze spacjami, zwracac raczej
obiekty: albo (Space ...) albo (LineBreak ...)

Wowczas tez sens moze miec to, zeby indeks lewego
nawiasu to bylo (reverse (indeks-wyrazenia 0 -1)),
|#

(define (cursor-next cursor::Cursor expression)
  ::Cursor
  (match cursor
    (`(,head . ,tail)
     (let* ((parent (cursor-ref expression tail))
	    (next (next-index head parent)))
       (if (equal? head next)
	   (cursor-next tail expression)
	   (hash-cons next tail))))
    (_
     cursor)))

(define (cursor-climb-front cursor::Cursor
			    expression)
  ::Cursor

  (define (climb-front cursor::Cursor target)
    ::Cursor
    (if (has-children? target)
	(let* ((index (first-index target))
	       (child (part-at index target)))
	  (if (eq? child target)
	      (if (and (pair? cursor)
		       (eq? (cursor-ref expression
					(tail
					 cursor))
			    target))
		  cursor
		  (hash-cons index cursor))
	      (climb-front (hash-cons index cursor)
			   child)))
	cursor))
    
  (climb-front cursor
	       (cursor-ref expression cursor)))

(define (cursor-back cursor::Cursor expression)
  ::Cursor
  (match cursor
    (`(,head . ,tail)
     (let* ((parent (cursor-ref expression tail))
	    (previous (previous-index head parent)))
       (if (equal? head previous)
	   (cursor-back tail expression)
	   (hash-cons previous tail))))
    (_
     cursor)))

(define (cursor-climb-back cursor::Cursor
			   expression)
  ::Cursor
  (define (climb-back cursor::Cursor target)::Cursor
    (if (has-children? target)
	(let* ((index (last-index target))
	       (child (part-at index target)))
	  (if (eq? child target)
	      (if (and (pair? cursor)
		       (eq? (cursor-ref expression
					(tail
					 cursor))
			    target))
		  cursor
		  (hash-cons index cursor))
	      (climb-back (hash-cons index cursor)
			  child)))
	cursor))
    
  (climb-back cursor
	      (cursor-ref expression cursor)))

;;   
;; (   (   a   b   )   )
;; ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^
;; 1 1 1 1 1 1 1 1 1 1 1
;; ( 0 1 1 1 1 1 1 1 2 )
;;     ( 0 1 2 3 4 )

#|
(e.g.
 (let ((context (head (parse-string
                       "  (   (   a   b   )   )"
                       ;; ^(#\()               ;
                       ;;  ^(0 0)              ;
                       ;;   ^(1 0)             ;
                       ;;    ^(2 0)            ;
                       ;;     ^(#\( 1)         ;
                       ;;      ^(0 0 1)        ;
                       ;;       ^(1 0 1)       ;
                       ;;        ^(2 0 1)      ;
                       ;;         ^(0 1 1)     ;
                       ;;          ^(0 2 1)    ;
                       ;;           ^(1 2 1)   ;
                       ;;            ^(2 2 1)  ;
                       ;;             ^(0 3 1) ;
                       ;;              ^(0 4 1);
                       ;;        (1 4 1)^      ;
                       ;;         (2 4 1)^     ;
                       ;;          (#\) 1)^    ;
                       ;;             (0 2)^   ;
                       ;;              (1 2)^  ;
                       ;;               (2 2)^ ;
                       ;;                (#\))^;
                       ))))
   (and)))
|#
