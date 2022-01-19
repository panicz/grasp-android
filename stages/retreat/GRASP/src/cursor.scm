(import (match))
(import (infix))
(import (examples))

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
;; normal boxes (or "lists"), even indices usually refer to spaces,
;; and odd indices refer to subsequent elements in a list.
;;
;; The exception is in the case of a dotted tail:
;; the odd index refers to the tail itself, as if it was
;; an element, and the next odd index refers to the
;; cdr of the list.
;;
;; Also, if the index is #\(, then it refers to the opening
;; parentehsis of a box, and if it is #\), it refers to its
;; closing parenthesis.

;; Every tile manages its own cursor values, so:
;; the source of every cursor value is a tile, which
;; controls the indices.
;;

(define-alias Cursor java.lang.Object) ;;gnu.lists.LList)

(define (drop k::integer #;elements-from s::list)::list
  (if (is k <= 0)
      s
      (drop (- k 1) #;elements-from (cdr s))))

(define (suffix? ending::list stem::list)::boolean
  (let ((m ::integer (length ending))
        (n ::integer (length stem)))
    (and (is m <= n)
         (let ((r ::integer (- n m)))
           (equal? (drop r stem) ending)))))
  
(define (subcursor cursor::Cursor context::Cursor)::Cursor
  (and cursor
       (is context suffix? cursor)))

;; inny pomysl na kursor jest taki, ze to ciag dowolnych
;; obiektow, czyli np. kombinatory moga miec takie "indeksy",
;; jak 'left czy 'right, ktore beda ze soba porownywane
;; za pomoca predykatu eqv?.

;; W kazdym razie jest tutaj jeszcze inny pomysl:
;; zeby zamiast stringa ze spacjami, zwracac raczej
;; obiekty: albo (Space ...) albo (LineBreak ...)

;; Wowczas tez sens moze miec to, zeby indeks lewego
;; nawiasu to bylo (reverse (indeks-wyrazenia 0 -1)),

;;   
;; (   (   a   b   )   )
;; ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^
;; 1 1 1 1 1 1 1 1 1 1 1
;; ( 0 1 1 1 1 1 1 1 2 )
;;     ( 0 1 2 3 4 )


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
