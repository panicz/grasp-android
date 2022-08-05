(define-alias CharSequence java.lang.CharSequence)

(define-alias Index java.lang.Object)

(define-alias Indexable* java.lang.Object)

;; we consider the Null class to be a class whose
;; only member #!null
(define-alias Null java.lang.Object)

;; Tile* means a Tile or a #!null, but currently Kawa's
;; type system doesn't let us express that (see (indexable)).
(define-alias Tile* java.lang.Object)

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

(define-alias Cursor java.lang.Object #;gnu.lists.LList)

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

#|
The specific functions that operate on cursor are
defined in the (cursor) module.
|#
