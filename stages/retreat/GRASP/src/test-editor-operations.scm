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
 (primitive)
 (extent)
 (text-painter)
 (combinators)
 (parse)
 (examples)
 (assert)
 (infix)
 (match)
 (functions)
 (print)
 (painter)
 (for)
 (document-operations)
 (editor-operations)
 (history)
 (painter)
 (text-painter)
 )

(define (snapshot)::String
  (parameterize ((the-painter (TextPainter)))
    (draw-document! (the-document))
    (let ((result ::String (invoke (the-painter) 'toString)))
      (display result)
      result)))

(set! (the-document)
  (call-with-input-string "\
(define (! n)
  (if (<= n 1)
    1
   (* n (! (- n 1)))))
" parse-document))

(e.g.
 (snapshot)
  ===> "
╭        ╭     ╮                   ╮
│ define │ ! n │                   │
│        ╰     ╯                   │
│   ╭    ╭        ╮              ╮ │
│   │ if │ <= n 1 │              │ │
│   │    ╰        ╯              │ │
│   │                            │ │
│   │     1                      │ │
│   │                            │ │
│   │    ╭     ╭   ╭       ╮ ╮ ╮ │ │
│   │    │ * n │ ! │ - n 1 │ │ │ │ │
╰   ╰    ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")

(let ((taken (take-cell! at: (cursor 3 3 1 1))))
  (splice! taken at: (cursor 0 4 1 1)))

;; this behaviour is wrong (but it's a start)

(e.g.
 (snapshot)
 ===> "
╭        ╭    ╮                     ╮
│ define │ !  │                     │
│        ╰    ╯                     │
│    ╭    ╭        ╮              ╮ │
│   n│ if │ <= n 1 │              │ │
│    │    ╰        ╯              │ │
│    │                            │ │
│    │     1                      │ │
│    │                            │ │
│    │    ╭     ╭   ╭       ╮ ╮ ╮ │ │
│    │    │ * n │ ! │ - n 1 │ │ │ │ │
╰    ╰    ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")
