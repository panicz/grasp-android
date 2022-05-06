(import
 (define-syntax-rule)
 (define-interface)
 (define-type)
 (define-object)
 (extent)
 (conversions)
 (indexable)
 (space)
 (cursor)
 (tile)
 (primitive)
 (extent)
 (text-screen)
 (combinators)
 (parse)
 (symbol)
 (examples)
 (assert)
 (infix)
 (match)
 (functions)
 (print)
 (screen)
 (for)
 (document-operations)
 (editor-operations)
 )

(define (rendered-with-cursor #!optional
			      (document (current-document)))
  (parameterize ((current-screen (TextScreen)))
    (let* ((target (cursor-ref))
	   (screen ::TextScreen (current-screen)))
      (draw-sequence! (head document))
      	  
      (screen:put! (if (is target instance? Space)
		       #\|
		       #\^)
		   (screen:remembered-top)
		   (screen:remembered-left))
      (screen:toString))))


;; "after" is a synonym for "begin" which assumes
;; that the last value is a test (i.e. a boolean expression)
(define-syntax-rule (after actions ... test)
  (begin actions ... test))


;; yields? is essentially a synonym for equal?, but when
;; there's only one argument given, it prints the value
;; to the screen and returns #t, which is useful for
;; capturing the result (it's a code-write-time convenience)
(define-syntax yields?
  (syntax-rules ()
    ((_ expression value)
     (equal? expression value))
    
    ((_ expression)
     (begin
       (display expression)
       (newline)
       #t))))

;; A few words about cursor: a document is implemented
;; as a pair whose first element is the actual contents
;; of the document, and whose second element is irrelevant
;; (but we set it to '()).
;;
;; A cursor is a path in a tree of nested sub-expressions,
;; but unlike, say, paths in UNIX, it is represented
;; from right to left (because this lets us avoid a lot
;; of consing, and also maximizes structural sharing via
;; hash-consing), so the last element of the list will
;; always be 1 (because that means the head of the top
;; list).
;;
;; The leftmost element, on the other hand, is a sub-unit
;; cursor position - it either points to a position within
;; a Space object, or a position within the Symbol object.
;; In case of lists, it can be #\[ or #\], meaning that it
;; points to either opening or closing paren of the
;; list object.
;;
;; For more information, see the `cursor.scm` file.

(e.g.
 (parameterize ((current-document (cons
				   (parse-string
				    "(define (square x) (* x x))")
				   '()))
		(current-cursor (cursor 1 1 3 1 1)))
   (and
    (string=? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰        ╰  ^       ╯ ╰       ╯ ╯
")
    (after
     (delete-forward!)
     (yields? (rendered-with-cursor) "
╭        ╭         ╮ ╭       ╮ ╮
│ define │ suare x │ │ * x x │ │
╰        ╰  ^      ╯ ╰       ╯ ╯
"))
    (after
     (delete-backward!)
     (yields? (rendered-with-cursor) "
╭        ╭        ╮ ╭       ╮ ╮
│ define │ uare x │ │ * x x │ │
╰        ╰ ^      ╯ ╰       ╯ ╯
"))
    (after
     (times 3 cursor-advance!)
     (yields? (rendered-with-cursor) "
╭        ╭        ╮ ╭       ╮ ╮
│ define │ uare x │ │ * x x │ │
╰        ╰    ^   ╯ ╰       ╯ ╯
"))
    (equal? (current-cursor) '(3 1 3 1 1))
    (after
     (insert-character! #\k)
     (yields? (rendered-with-cursor) "
╭        ╭         ╮ ╭       ╮ ╮
│ define │ uarke x │ │ * x x │ │
╰        ╰     ^   ╯ ╰       ╯ ╯
"))
    (yields? (cursor-ref) 'uarke)
    (after
     (cursor-advance!)
     (yields? (rendered-with-cursor) "
╭        ╭         ╮ ╭       ╮ ╮
│ define │ uarke x │ │ * x x │ │
╰        ╰      |  ╯ ╰       ╯ ╯
"))
    (yields? (cursor-ref) (Space fragments: '(1)))
    
    #;(after
     (insert-character! #\z)
     (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ uarkez x │ │ * x x │ │
╰        ╰       |  ╯ ╰       ╯ ╯
"))
    (after
     (times 6 cursor-retreat!)
     (yields? (rendered-with-cursor) "
╭        ╭         ╮ ╭       ╮ ╮
│ define │ uarke x │ │ * x x │ │
╰        ╰ |       ╯ ╰       ╯ ╯
"))
    ;(yields? (cursor-ref) 'uarkez)
    
    )))
