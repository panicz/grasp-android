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

(define input "\
(define (factorial n)
  (if (<= n 0)
      1
      (* n (! (- n 1))))) 
(e.g. (factorial 5) ===> 120)
")


(define (rendered #!optional (document (current-document)))
  (parameterize ((current-screen (TextScreen)))
    (let* ((target (cursor-ref))
	   (screen ::TextScreen (current-screen)))
      (draw-sequence! (head document))
      (screen:put! #\^ (screen:remembered-top)
		   (screen:remembered-left))
      (invoke (current-screen) 'toString))))

(e.g.
 (parameterize ((current-cursor (recons* 1 1 3 1 1 '()))
		(current-document (cons (parse-string input) '())))
   (and
    (string=? (rendered) "
╭        ╭             ╮              ╮
│ define │ factorial n │              │
│        ╰  ^          ╯              │
│   ╭    ╭        ╮                 ╮ │
│   │ if │ <= n 0 │                 │ │
│   │    ╰        ╯                 │ │
│   │                               │ │
│   │       1                       │ │
│   │                               │ │
│   │       ╭     ╭   ╭       ╮ ╮ ╮ │ │
│   │       │ * n │ ! │ - n 1 │ │ │ │ │
╰   ╰       ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
╭      ╭             ╮          ╮      
│ e.g. │ factorial 5 │ ===> 120 │      
╰      ╰             ╯          ╯      
")
    (begin
      (delete-forward!)
      (string=? (rendered) "
╭        ╭            ╮               ╮
│ define │ fctorial n │               │
│        ╰  ^         ╯               │
│   ╭    ╭        ╮                 ╮ │
│   │ if │ <= n 0 │                 │ │
│   │    ╰        ╯                 │ │
│   │                               │ │
│   │       1                       │ │
│   │                               │ │
│   │       ╭     ╭   ╭       ╮ ╮ ╮ │ │
│   │       │ * n │ ! │ - n 1 │ │ │ │ │
╰   ╰       ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
╭      ╭             ╮          ╮      
│ e.g. │ factorial 5 │ ===> 120 │      
╰      ╰             ╯          ╯      
"))
    (begin
      (delete-backward!)
      (string=? (rendered) "
╭        ╭           ╮                ╮
│ define │ ctorial n │                │
│        ╰ ^         ╯                │
│   ╭    ╭        ╮                 ╮ │
│   │ if │ <= n 0 │                 │ │
│   │    ╰        ╯                 │ │
│   │                               │ │
│   │       1                       │ │
│   │                               │ │
│   │       ╭     ╭   ╭       ╮ ╮ ╮ │ │
│   │       │ * n │ ! │ - n 1 │ │ │ │ │
╰   ╰       ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
╭      ╭             ╮          ╮      
│ e.g. │ factorial 5 │ ===> 120 │      
╰      ╰             ╯          ╯      
"))
    )))
