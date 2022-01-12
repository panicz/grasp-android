
;; A Cursor is a list of things that can be used for
;; indexing tiles. Those can be any objects that can be
;; compared using the 'eqv?' predicate, but in practice
;; those values can either be integers, symbols or
;; characters.
;;
;; The order of elements in the cursor list is such that
;; the first element of the list is an index of the
;; innermost element relative to its parent (which is
;; pointed to by the second element of the list, or
;; is the currently considered node if there isn't one)
;;
;; This order doesn't allow to select the expression pointed to by
;; a cursor in a tail-recursive manner: we need to reach the last
;; index in order to choose a sub-expression on a top-level.
;;
;; The reason we chose this "reverse" order has to do with
;; the way we build those indices: we start from a top level,
;; and we descend deeper recursively; therefore, we "cons"
;; the inermost expressions' indices last.
;;
;; Also, this strategy maximizes "structural sharing"
;; between cursors to different expressions
;; (which I think is beautiful), and reverting this
;; order would be wasteful
;;
;; Another thing with cursors is that, when refering to
;; normal boxes,  even indices usually refer to spaces,
;; and odd indices refer to subsequent elements in a list.
;;
;; The exception is in the case of a dotted tail:
;; the odd index refers to the tail itself, as if it was
;; an element, and the next odd index refers to the
;; cdr of the list.

(define-alias Cursor gnu.lists.LList)


