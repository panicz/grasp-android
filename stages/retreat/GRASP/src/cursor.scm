(import (define-interface))
(import (define-type))
(import (define-cache))
(import (infix))
(import (indexable))
(import (match))
(import (space))
(import (assert))
(import (functions))
(import (print))

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

Also, if the index is #\[, then it refers to the 
opening parentehsis of a box, and if it is #\], 
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

(define (cell-index cell::pair index::int)::Indexable*
  (assert (is index >= 0))
  (cond ((= index 0)
         (pre-head-space cell))
        ((= index 1)
	 (car cell))
        ((= index 2)
         (post-head-space cell))
        ((dotted? cell)
         (cond ((= index 3)
                (head-tail-separator cell))
               ((= index 4)
                (pre-tail-space cell))
               ((= index 5)
		(cdr cell))
               ((= index 6)
                (post-tail-space cell))))
        (else
         (cell-index (cdr cell) (- index 2)))))

(define (set-cell-index! cell::pair index::int value)
  (assert (is index >= 0))
  (cond ((= index 0)
         (set! (pre-head-space cell) value))
        ((= index 1)
	 (set! (car cell) value))
        ((= index 2)
         (set! (post-head-space cell) value))
        ((dotted? cell)
         (cond ((= index 3)
                (set! (head-tail-separator cell) value))
               ((= index 4)
                (set! (pre-tail-space cell) value))
               ((= index 5)
		(set! (cdr cell) value))
               ((= index 6)
                (set! (post-tail-space cell) value))))
        (else
         (set-cell-index! (cdr cell) (- index 2) value))))

(set! (setter cell-index) set-cell-index!)

(define (last-cell-index cell::list
			 #!optional
			 (initial::int 2))
  ::int
  (cond
   ((null? cell) 0)
   
   ((dotted? cell)
    (+ initial 4))
   ((pair? (tail cell))
    (last-cell-index (tail cell)
		     (+ initial 2)))
   
   (else
    initial)))

(define (part-at index::Index object)::Indexable*
  (cond ((Indexable? object)
	 (invoke (as Indexable object)
		 'part-at index))

	((pair? object)
	 (if (or (eq? index #\[) (eq? index #\]))
	     object
	     (cell-index object (as int index))))
	 
	(else
	 object
	 #;(error "Don't know how to extract "index
		" from "object))))


(define (cursor-ref tile cursor::Cursor)
  (match cursor
    ('()
     tile)
    (`(,head . ,tail)
     (let ((parent (cursor-ref tile tail)))
       (if (and (null? parent)
		(number? head)
		(pair? tail))
	   (let* ((grand (cursor-ref tile (cdr tail)))
		  (space (null-space-ref grand (car tail))))
	     space)
	   (part-at head parent))))
    (_
     (error "Unable to refer to cursor "cursor
	    " in "tile))))

(define (first-index object)
  (cond ((Indexable? object)
	 (invoke (as Indexable object)
		 'first-index))

	((string? object)
	 0)

	((or (pair? object) (null? object))
	 #\[)

	(else
	 (error "Don't know how to obtain first index from "
		object))))

(define (last-index object)
  (cond ((Indexable? object)
	 (invoke (as Indexable object) 'last-index))

	((string? object)
	 (string-length (as string object)))

	((or (pair? object) (null? object))
	 #\])
	
	(else
	 (error "Don't know how to obtain last index from "
		object))))

(define (next-index index::Index object)::Index
  (cond ((Indexable? object)
	 (invoke (as Indexable object)
		 'next-index index))

	((string? object)
	 (min (last-index object) (+ index 1)))

	((or (pair? object) (null? object))
	 (match index
	   (#\[ 0)
	   (#\] #\])
	   (,@(is _ < (last-cell-index object))
	    (+ index 1))
	   (_
	    #\])))
	
	(else
	 (error "Don't know how to obtain next index to "
		index" in "object))))

(define (previous-index index::Index object)::Index
  (cond ((Indexable? object)
	 (invoke (as Indexable object)
		 'previous-index index))

	((string? object)
	 (max 0 (- index 1)))
	
	((or (pair? object) (null? object))
	 (match index
	   (0 #\[)
	   (#\] (last-cell-index object))
	   (#\[ #\[)
	   (_ (- index 1))))
	
	(else
	 (error "Don't know how to obtain previous index to "
		index " in "object))))

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
		       child))))
    
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
		      child))))
  (climb-back cursor
	      (cursor-ref expression cursor)))  



(define (cursor-advance cursor document)
  (define (next cursor)
    (cursor-climb-front
     (cursor-next cursor
		  document)
     document))
  (let* ((updated (next cursor))
	 (target (cursor-ref document updated))
	 (limit (last-index target)))
    (cond
     ((and (isnt target pair?)
	   (eqv? (head updated) limit))
      (next updated))
     (else
      updated))))
  

(define (cursor-retreat cursor document)
  (define (next cursor)
    (cursor-climb-back
     (cursor-back cursor
		  document)
     document))
  (let* ((updated (next cursor))
	 (target (cursor-ref document updated))
	 (limit (first-index target)))
    (cond
     ((and (isnt target pair?)
	   (eqv? (head updated) limit))
      (next updated))
     (else
      updated))))

  

;; We stipulate that, for any N >= 1, () < (x1 ... xN)
;; (as a consequence, whenever one cursor is a proper
;; suffix of another, it is considered to be "earlier"
;; than the longer one)

(define (cursor< a::Cursor b::Cursor document::Indexable)
  ::boolean
  (define (k< k::int a*::Cursor b*::Cursor parent::Indexable)
    ::boolean
    (if (is k < 0)
	#f
	(let* ((a/k (a* k))
	       (b/k (b* k)))
	  (if (eqv? a/k b/k)
	      (k< (- k 1) a* b* (parent:part-at a/k))
	      (parent:index< a/k b/k)))))
  
  (let* ((length/a (length a))
	 (length/b (length b)))
    (cond ((is length/a < length/b)
	   (let ((stem/b (drop (- length/b length/a) b)))
	     (k< (- length/a 1) a stem/b document)))
	     
	  ((is length/a = length/b)
	   (k< (- length/a 1) a b document))
    
	  ((is length/a > length/b)
	   (not (cursor< b a document))))))
