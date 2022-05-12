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
			      (document (the-document))
			      (cursor (the-cursor)))
  (parameterize ((the-screen (TextScreen)))
    (let* ((target (cursor-ref document cursor))
	   (screen ::TextScreen (the-screen)))
      (draw-sequence! (head document)
		      screen: screen
		      cursor: cursor)
      	  
      (screen:put! (if (is target instance? Space)
		       #\|
		       #\^)
		   (screen:remembered-top)
		   (screen:remembered-left))
      (screen:toString))))

(define-syntax snapshot
  (syntax-rules ()
    ((_ state)
     (assert (equal? (rendered-with-cursor) state)))
    ((_)
     (display (rendered-with-cursor)))))


(define original-document (the-document))
(define original-cursor (the-cursor))

(set! (the-document)
      ;; we're using (cons _ '()) rather than "list", because
      ;; Kawa's cons cells provide equal?-like equals method
      ;; which makes them work poorly with weak hash tables.
      ;;
      ;; We need to patch Kawa in order to resolve the problem,
      ;; but for now we just stick with what's in the repo.
      (cons (parse-string "") '()))

(set! (the-cursor) (cursor 0 0 1))

(snapshot "
|
")

(insert-character! #\[)

(snapshot "
╭   ╮
│   │
╰ | ╯
")

(delete-backward!)

(snapshot "


|
")

(insert-character! #\[)

(snapshot "
╭  ╮
│  │
╰| ╯
")

(for-each insert-character! '(#\d #\e #\f #\i #\n #\e))

(snapshot "
╭        ╮
│ define │
╰       ^╯
")

(insert-character! #\space)
(insert-character! #\[)

(snapshot "
╭        ╭  ╮ ╮
│ define │  │ │
╰        ╰| ╯ ╯
")

(for-each insert-character! '(#\m #\a #\p))

(snapshot "
╭        ╭     ╮ ╮
│ define │ map │ │
╰        ╰    ^╯ ╯
")

(insert-character! #\space)

(snapshot "
╭        ╭      ╮ ╮
│ define │ map  │ │
╰        ╰     |╯ ╯
")

(for-each insert-character! '(#\f #\space #\l))

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
╰        ╰        ^╯ ╯
")

(times 2 cursor-advance!)

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
╰        ╰         ^ ╯
")

(insert-character! #\[)

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
╰        ^         ╯ ╯
")

(insert-character! #\])

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
╰        ╰         ^ ╯
")

(cursor-advance!)

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
╰        ╰         ╯|╯
")

(insert-character! #\newline)

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
│        ╰         ╯ │
│                    │
╰|                   ╯
")

(insert-character! #\[)

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
│        ╰         ╯ │
│ ╭  ╮               │
│ │  │               │
╰ ╰| ╯               ╯
")

(for-each insert-character! '(#\m #\a #\t #\c #\h
			      #\space
			      #\l))

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
│        ╰         ╯ │
│ ╭         ╮        │
│ │ match l │        │
╰ ╰         ╯        ╯
")

(insert-character! #\newline)

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
│        ╰         ╯ │
│ ╭         ╮        │
│ │ match l │        │
│ │         │        │
│ │         │        │
│ │         │        │
╰ ╰|        ╯        ╯
")

(insert-character! #\[)

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
│        ╰         ╯ │
│ ╭         ╮        │
│ │ match l │        │
│ │         │        │
│ │ ╭  ╮    │        │
│ │ │  │    │        │
╰ ╰ ╰| ╯    ╯        ╯
")

(insert-character! #\[)

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
│        ╰         ╯ │
│ ╭          ╮       │
│ │ match l  │       │
│ │          │       │
│ │ ╭ ╭  ╮ ╮ │       │
│ │ │ │  │ │ │       │
╰ ╰ ╰ ╰| ╯ ╯ ╯       ╯
")

(cursor-retreat!)

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
│        ╰         ╯ │
│ ╭          ╮       │
│ │ match l  │       │
│ │          │       │
│ │ ╭ ╭  ╮ ╮ │       │
│ │ │ │  │ │ │       │
╰ ╰ ╰ ^  ╯ ╯ ╯       ╯
")

(insert-character! #\')

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
│        ╰         ╯ │
│ ╭          ╮       │
│ │ match l  │       │
│ │          │       │
│ │ ╭ ┏  ┓ ╮ │       │
│ │ │ ┃  ┃ │ │       │
╰ ╰ ╰ ^  ┛ ╯ ╯       ╯
")

(copy-expression!)
(insert-character! #\])
(cursor-advance!)
(insert-character! #\space)

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
│        ╰         ╯ │
│ ╭           ╮      │
│ │ match l   │      │
│ │           │      │
│ │ ╭ ┏  ┓  ╮ │      │
│ │ │ ┃  ┃  │ │      │
╰ ╰ ╰ ┗  ┛ |╯ ╯      ╯
")

(paste-expression!)
(times 2 insert-character! #\])
(cursor-advance!)

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
│        ╰         ╯ │
│ ╭               ╮  │
│ │ match l       │  │
│ │               │  │
│ │ ╭ ┏  ┓ ┏  ┓ ╮ │  │
│ │ │ ┃  ┃ ┃  ┃ │ │  │
╰ ╰ ╰ ┗  ┛ ┗  ┛ ╯|╯  ╯
")

(insert-character! #\newline)

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
│        ╰         ╯ │
│ ╭               ╮  │
│ │ match l       │  │
│ │               │  │
│ │ ╭ ┏  ┓ ┏  ┓ ╮ │  │
│ │ │ ┃  ┃ ┃  ┃ │ │  │
│ │ ╰ ┗  ┛ ┗  ┛ ╯ │  │
│ │               │  │
│ │               │  │
╰ ╰|              ╯  ╯
")

(times 2 insert-character! #\[)

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
│        ╰         ╯ │
│ ╭               ╮  │
│ │ match l       │  │
│ │               │  │
│ │ ╭ ┏  ┓ ┏  ┓ ╮ │  │
│ │ │ ┃  ┃ ┃  ┃ │ │  │
│ │ ╰ ┗  ┛ ┗  ┛ ╯ │  │
│ │ ╭ ╭  ╮ ╮      │  │
│ │ │ │  │ │      │  │
╰ ╰ ╰ ^  ╯ ╯      ╯  ╯
")

(insert-character! #\`)

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
│        ╰         ╯ │
│ ╭               ╮  │
│ │ match l       │  │
│ │               │  │
│ │ ╭ ┏  ┓ ┏  ┓ ╮ │  │
│ │ │ ┃  ┃ ┃  ┃ │ │  │
│ │ ╰ ┗  ┛ ┗  ┛ ╯ │  │
│ │ ╭ ╓  ╖ ╮      │  │
│ │ │ ║  ║ │      │  │
╰ ╰ ╰ ^  ╜ ╯      ╯  ╯
")

(insert-character! #\space)

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
│        ╰         ╯ │
│ ╭               ╮  │
│ │ match l       │  │
│ │               │  │
│ │ ╭ ┏  ┓ ┏  ┓ ╮ │  │
│ │ │ ┃  ┃ ┃  ┃ │ │  │
│ │ ╰ ┗  ┛ ┗  ┛ ╯ │  │
│ │ ╭ ╓  ╖ ╮      │  │
│ │ │ ║  ║ │      │  │
╰ ╰ ╰ ╙| ╜ ╯      ╯  ╯
")

(for-each insert-character! '(#\h #\e #\a #\d))

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
│        ╰         ╯ │
│ ╭               ╮  │
│ │ match l       │  │
│ │               │  │
│ │ ╭ ┏  ┓ ┏  ┓ ╮ │  │
│ │ │ ┃  ┃ ┃  ┃ │ │  │
│ │ ╰ ┗  ┛ ┗  ┛ ╯ │  │
│ │ ╭ ╓      ╖ ╮  │  │
│ │ │ ║ head ║ │  │  │
╰ ╰ ╰ ╙     ^╜ ╯  ╯  ╯
")

(for-each insert-character! '(#\space #\t #\a #\i #\l))

(snapshot "
╭        ╭         ╮ ╮
│ define │ map f l │ │
│        ╰         ╯ │
│ ╭               ╮  │
│ │ match l       │  │
│ │               │  │
│ │ ╭ ┏  ┓ ┏  ┓ ╮ │  │
│ │ │ ┃  ┃ ┃  ┃ │ │  │
│ │ ╰ ┗  ┛ ┗  ┛ ╯ │  │
│ │ ╭ ╓      ╖ ╮  │  │
│ │ │ ║ head ║ │  │  │
╰ ╰ ╰ ╙     ^╜ ╯  ╯  ╯
")

(snapshot "
╭        ╭         ╮    ╮
│ define │ map f l │    │
│        ╰         ╯    │
│ ╭                   ╮ │
│ │ match l           │ │
│ │                   │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮     │ │
│ │ │ ┃  ┃ ┃  ┃ │     │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯     │ │
│ │ ╭ ╓           ╖ ╮ │ │
│ │ │ ║ head tail ║ │ │ │
╰ ╰ ╰ ╙          ^╜ ╯ ╯ ╯
")

(backward-expression!)

(snapshot "
╭        ╭         ╮    ╮
│ define │ map f l │    │
│        ╰         ╯    │
│ ╭                   ╮ │
│ │ match l           │ │
│ │                   │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮     │ │
│ │ │ ┃  ┃ ┃  ┃ │     │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯     │ │
│ │ ╭ ╓           ╖ ╮ │ │
│ │ │ ║ head tail ║ │ │ │
╰ ╰ ╰ ╙     |     ╜ ╯ ╯ ╯
")

(insert-character! #\|)

(snapshot "
╭        ╭         ╮      ╮
│ define │ map f l │      │
│        ╰         ╯      │
│ ╭                     ╮ │
│ │ match l             │ │
│ │                     │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮       │ │
│ │ │ ┃  ┃ ┃  ┃ │       │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯       │ │
│ │ ╭ ╓      ╥      ╖ ╮ │ │
│ │ │ ║ head ║ tail ║ │ │ │
╰ ╰ ╰ ╙      ^      ╜ ╯ ╯ ╯
")

(forward-expression!)
(times 2 cursor-advance!)

(snapshot "
╭        ╭         ╮      ╮
│ define │ map f l │      │
│        ╰         ╯      │
│ ╭                     ╮ │
│ │ match l             │ │
│ │                     │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮       │ │
│ │ │ ┃  ┃ ┃  ┃ │       │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯       │ │
│ │ ╭ ╓      ╥      ╖ ╮ │ │
│ │ │ ║ head ║ tail ║ │ │ │
╰ ╰ ╰ ╙      ╨      ╜|╯ ╯ ╯
")

(insert-character! #\newline)

(snapshot "
╭        ╭         ╮      ╮
│ define │ map f l │      │
│        ╰         ╯      │
│ ╭                     ╮ │
│ │ match l             │ │
│ │                     │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮       │ │
│ │ │ ┃  ┃ ┃  ┃ │       │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯       │ │
│ │ ╭ ╓      ╥      ╖ ╮ │ │
│ │ │ ║ head ║ tail ║ │ │ │
│ │ │ ╙      ╨      ╜ │ │ │
│ │ │                 │ │ │
│ │ │                 │ │ │
╰ ╰ ╰|                ╯ ╯ ╯
")

(move-cursor-up!)


(snapshot "
╭        ╭         ╮      ╮
│ define │ map f l │      │
│        ╰         ╯      │
│ ╭                     ╮ │
│ │ match l             │ │
│ │                     │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮       │ │
│ │ │ ┃  ┃ ┃  ┃ │       │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯       │ │
│ │ ╭ ╓      ╥      ╖ ╮ │ │
│ │ │ ║ head ║ tail ║ │ │ │
│ │ │|╙      ╨      ╜ │ │ │
│ │ │                 │ │ │
│ │ │                 │ │ │
╰ ╰ ╰                 ╯ ╯ ╯
")

(cursor-advance!)

(snapshot "
╭        ╭         ╮      ╮
│ define │ map f l │      │
│        ╰         ╯      │
│ ╭                     ╮ │
│ │ match l             │ │
│ │                     │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮       │ │
│ │ │ ┃  ┃ ┃  ┃ │       │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯       │ │
│ │ ╭ ╓      ╥      ╖ ╮ │ │
│ │ │ ║ head ║ tail ║ │ │ │
│ │ │ ^      ╨      ╜ │ │ │
│ │ │                 │ │ │
│ │ │                 │ │ │
╰ ╰ ╰                 ╯ ╯ ╯
")

(copy-expression!)
(move-cursor-down!)

(snapshot "
╭        ╭         ╮      ╮
│ define │ map f l │      │
│        ╰         ╯      │
│ ╭                     ╮ │
│ │ match l             │ │
│ │                     │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮       │ │
│ │ │ ┃  ┃ ┃  ┃ │       │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯       │ │
│ │ ╭ ╓      ╥      ╖ ╮ │ │
│ │ │ ║ head ║ tail ║ │ │ │
│ │ │ ╙      ╨      ╜ │ │ │
│ │ │                 │ │ │
│ │ │                 │ │ │
╰ ╰ ╰|                ╯ ╯ ╯
")

(paste-expression!)

(snapshot "
╭        ╭         ╮      ╮
│ define │ map f l │      │
│        ╰         ╯      │
│ ╭                     ╮ │
│ │ match l             │ │
│ │                     │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮       │ │
│ │ │ ┃  ┃ ┃  ┃ │       │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯       │ │
│ │ ╭ ╓      ╥      ╖ ╮ │ │
│ │ │ ║ head ║ tail ║ │ │ │
│ │ │ ╙      ╨      ╜ │ │ │
│ │ │ ╓      ╥      ╖ │ │ │
│ │ │ ║ head ║ tail ║ │ │ │
╰ ╰ ╰ ╙      ╨      ╜|╯ ╯ ╯
")

(cursor-retreat!)
(insert-character! #\[)
(cursor-advance!)
(select-next-expression!)

(snapshot "
╭        ╭         ╮      ╮
│ define │ map f l │      │
│        ╰         ╯      │
│ ╭                     ╮ │
│ │ match l             │ │
│ │                     │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮       │ │
│ │ │ ┃  ┃ ┃  ┃ │       │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯       │ │
│ │ ╭ ╓      ╥      ╖ ╮ │ │
│ │ │ ║ head ║ tail ║ │ │ │
│ │ │ ╙      ╨      ╜ │ │ │
│ │ │ ╓      ╥      ╖ │ │ │
│ │ │ ║ head ║ tail ║ │ │ │
╰ ╰ ╰ ╙ ~~~~^╨      ╜ ╯ ╯ ╯
")

(insert-character! #\[)

(snapshot "
╭        ╭         ╮          ╮
│ define │ map f l │          │
│        ╰         ╯          │
│ ╭                         ╮ │
│ │ match l                 │ │
│ │                         │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮           │ │
│ │ │ ┃  ┃ ┃  ┃ │           │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯           │ │
│ │ ╭ ╓      ╥      ╖     ╮ │ │
│ │ │ ║ head ║ tail ║     │ │ │
│ │ │ ╙      ╨      ╜     │ │ │
│ │ │ ╓ ╭      ╮ ╥      ╖ │ │ │
│ │ │ ║ │ head │ ║ tail ║ │ │ │
╰ ╰ ╰ ╙ ╰ ~~~~^╯ ╨      ╜ ╯ ╯ ╯
")

(move-cursor-left!)

(snapshot "
╭        ╭         ╮          ╮
│ define │ map f l │          │
│        ╰         ╯          │
│ ╭                         ╮ │
│ │ match l                 │ │
│ │                         │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮           │ │
│ │ │ ┃  ┃ ┃  ┃ │           │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯           │ │
│ │ ╭ ╓      ╥      ╖     ╮ │ │
│ │ │ ║ head ║ tail ║     │ │ │
│ │ │ ╙      ╨      ╜     │ │ │
│ │ │ ╓ ╭      ╮ ╥      ╖ │ │ │
│ │ │ ║ │ head │ ║ tail ║ │ │ │
╰ ╰ ╰ ╙ ╰|     ╯ ╨      ╜ ╯ ╯ ╯
")

(for-each insert-character! '(#\f #\space))

(snapshot "
╭        ╭         ╮            ╮
│ define │ map f l │            │
│        ╰         ╯            │
│ ╭                           ╮ │
│ │ match l                   │ │
│ │                           │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮             │ │
│ │ │ ┃  ┃ ┃  ┃ │             │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯             │ │
│ │ ╭ ╓      ╥      ╖       ╮ │ │
│ │ │ ║ head ║ tail ║       │ │ │
│ │ │ ╙      ╨      ╜       │ │ │
│ │ │ ╓ ╭        ╮ ╥      ╖ │ │ │
│ │ │ ║ │ f head │ ║ tail ║ │ │ │
╰ ╰ ╰ ╙ ╰  |     ╯ ╨      ╜ ╯ ╯ ╯
")

(insert-character! #\])
(times 3 cursor-advance!)

(snapshot "
╭        ╭         ╮            ╮
│ define │ map f l │            │
│        ╰         ╯            │
│ ╭                           ╮ │
│ │ match l                   │ │
│ │                           │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮             │ │
│ │ │ ┃  ┃ ┃  ┃ │             │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯             │ │
│ │ ╭ ╓      ╥      ╖       ╮ │ │
│ │ │ ║ head ║ tail ║       │ │ │
│ │ │ ╙      ╨      ╜       │ │ │
│ │ │ ╓ ╭        ╮ ╥      ╖ │ │ │
│ │ │ ║ │ f head │ ║ tail ║ │ │ │
╰ ╰ ╰ ╙ ╰        ╯ ╨|     ╜ ╯ ╯ ╯
")

(select-next-expression!)

(snapshot "
╭        ╭         ╮            ╮
│ define │ map f l │            │
│        ╰         ╯            │
│ ╭                           ╮ │
│ │ match l                   │ │
│ │                           │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮             │ │
│ │ │ ┃  ┃ ┃  ┃ │             │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯             │ │
│ │ ╭ ╓      ╥      ╖       ╮ │ │
│ │ │ ║ head ║ tail ║       │ │ │
│ │ │ ╙      ╨      ╜       │ │ │
│ │ │ ╓ ╭        ╮ ╥      ╖ │ │ │
│ │ │ ║ │ f head │ ║ tail ║ │ │ │
╰ ╰ ╰ ╙ ╰        ╯ ╨ ~~~~^╜ ╯ ╯ ╯
")

(insert-character! #\[)


(snapshot "
╭        ╭         ╮                ╮
│ define │ map f l │                │
│        ╰         ╯                │
│ ╭                               ╮ │
│ │ match l                       │ │
│ │                               │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮                 │ │
│ │ │ ┃  ┃ ┃  ┃ │                 │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯                 │ │
│ │ ╭ ╓      ╥      ╖           ╮ │ │
│ │ │ ║ head ║ tail ║           │ │ │
│ │ │ ╙      ╨      ╜           │ │ │
│ │ │ ╓ ╭        ╮ ╥ ╭      ╮ ╖ │ │ │
│ │ │ ║ │ f head │ ║ │ tail │ ║ │ │ │
╰ ╰ ╰ ╙ ╰        ╯ ╨ ╰ ~~~~^╯ ╜ ╯ ╯ ╯
")

(move-cursor-left!)
(for-each insert-character! '(#\m #\a #\p #\space #\f #\space))

(snapshot "
╭        ╭         ╮                      ╮
│ define │ map f l │                      │
│        ╰         ╯                      │
│ ╭                                     ╮ │
│ │ match l                             │ │
│ │                                     │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮                       │ │
│ │ │ ┃  ┃ ┃  ┃ │                       │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯                       │ │
│ │ ╭ ╓      ╥      ╖                 ╮ │ │
│ │ │ ║ head ║ tail ║                 │ │ │
│ │ │ ╙      ╨      ╜                 │ │ │
│ │ │ ╓ ╭        ╮ ╥ ╭            ╮ ╖ │ │ │
│ │ │ ║ │ f head │ ║ │ map f tail │ ║ │ │ │
╰ ╰ ╰ ╙ ╰        ╯ ╨ ╰      |     ╯ ╜ ╯ ╯ ╯
")

(move-cursor-up!)
(times 2 cursor-retreat!)


(snapshot "
╭        ╭         ╮                      ╮
│ define │ map f l │                      │
│        ╰         ╯                      │
│ ╭                                     ╮ │
│ │ match l                             │ │
│ │                                     │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮                       │ │
│ │ │ ┃  ┃ ┃  ┃ │                       │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯                       │ │
│ │ ╭ ╓      ╥      ╖                 ╮ │ │
│ │ │ ║ head ║ tail ║                 │ │ │
│ │ │ ╙      ╨     |╜                 │ │ │
│ │ │ ╓ ╭        ╮ ╥ ╭            ╮ ╖ │ │ │
│ │ │ ║ │ f head │ ║ │ map f tail │ ║ │ │ │
╰ ╰ ╰ ╙ ╰        ╯ ╨ ╰            ╯ ╜ ╯ ╯ ╯
")

(times 2 backward-expression!)

(snapshot "
╭        ╭         ╮                      ╮
│ define │ map f l │                      │
│        ╰         ╯                      │
│ ╭                                     ╮ │
│ │ match l                             │ │
│ │                                     │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮                       │ │
│ │ │ ┃  ┃ ┃  ┃ │                       │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯                       │ │
│ │ ╭ ╓      ╥      ╖                 ╮ │ │
│ │ │ ║ head ║ tail ║                 │ │ │
│ │ │ ╙     |╨      ╜                 │ │ │
│ │ │ ╓ ╭        ╮ ╥ ╭            ╮ ╖ │ │ │
│ │ │ ║ │ f head │ ║ │ map f tail │ ║ │ │ │
╰ ╰ ╰ ╙ ╰        ╯ ╨ ╰            ╯ ╜ ╯ ╯ ╯
")

(insert-character! #\newline)

(snapshot "
╭        ╭         ╮                      ╮
│ define │ map f l │                      │
│        ╰         ╯                      │
│ ╭                                     ╮ │
│ │ match l                             │ │
│ │                                     │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮                       │ │
│ │ │ ┃  ┃ ┃  ┃ │                       │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯                       │ │
│ │ ╭ ╓         ╖                     ╮ │ │
│ │ │ ║ head    ║                     │ │ │
│ │ │ ║         ║                     │ │ │
│ │ │ ║  ╥      ║                     │ │ │
│ │ │ ║  ║ tail ║                     │ │ │
│ │ │ ╙| ╨      ╜                     │ │ │
│ │ │ ╓ ╭        ╮ ╥ ╭            ╮ ╖ │ │ │
│ │ │ ║ │ f head │ ║ │ map f tail │ ║ │ │ │
╰ ╰ ╰ ╙ ╰        ╯ ╨ ╰            ╯ ╜ ╯ ╯ ╯
")

(times 2 cursor-advance!)

(snapshot "
╭        ╭         ╮                      ╮
│ define │ map f l │                      │
│        ╰         ╯                      │
│ ╭                                     ╮ │
│ │ match l                             │ │
│ │                                     │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮                       │ │
│ │ │ ┃  ┃ ┃  ┃ │                       │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯                       │ │
│ │ ╭ ╓         ╖                     ╮ │ │
│ │ │ ║ head    ║                     │ │ │
│ │ │ ║         ║                     │ │ │
│ │ │ ║  ╥      ║                     │ │ │
│ │ │ ║  ║ tail ║                     │ │ │
│ │ │ ╙  ╨|     ╜                     │ │ │
│ │ │ ╓ ╭        ╮ ╥ ╭            ╮ ╖ │ │ │
│ │ │ ║ │ f head │ ║ │ map f tail │ ║ │ │ │
╰ ╰ ╰ ╙ ╰        ╯ ╨ ╰            ╯ ╜ ╯ ╯ ╯
")

(insert-character! #\newline)


(snapshot "
╭        ╭         ╮                      ╮
│ define │ map f l │                      │
│        ╰         ╯                      │
│ ╭                                     ╮ │
│ │ match l                             │ │
│ │                                     │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮                       │ │
│ │ │ ┃  ┃ ┃  ┃ │                       │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯                       │ │
│ │ ╭ ╓      ╖                        ╮ │ │
│ │ │ ║ head ║                        │ │ │
│ │ │ ║ ____ ║                        │ │ │
│ │ │ ║      ║                        │ │ │
│ │ │ ║ tail ║                        │ │ │
│ │ │ ╙|     ╜                        │ │ │
│ │ │ ╓ ╭        ╮ ╥ ╭            ╮ ╖ │ │ │
│ │ │ ║ │ f head │ ║ │ map f tail │ ║ │ │ │
╰ ╰ ╰ ╙ ╰        ╯ ╨ ╰            ╯ ╜ ╯ ╯ ╯
")

(move-cursor-down!)

(snapshot "
╭        ╭         ╮                      ╮
│ define │ map f l │                      │
│        ╰         ╯                      │
│ ╭                                     ╮ │
│ │ match l                             │ │
│ │                                     │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮                       │ │
│ │ │ ┃  ┃ ┃  ┃ │                       │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯                       │ │
│ │ ╭ ╓      ╖                        ╮ │ │
│ │ │ ║ head ║                        │ │ │
│ │ │ ║ ____ ║                        │ │ │
│ │ │ ║      ║                        │ │ │
│ │ │ ║ tail ║                        │ │ │
│ │ │ ╙      ╜                        │ │ │
│ │ │ ╓ ╭        ╮ ╥ ╭            ╮ ╖ │ │ │
│ │ │ ║ │ f head │ ║ │ map f tail │ ║ │ │ │
╰ ╰ ╰ ╙|╰        ╯ ╨ ╰            ╯ ╜ ╯ ╯ ╯
")

(cursor-advance!)
(insert-character! #\])
(times 2 cursor-advance!)

(snapshot "
╭        ╭         ╮                      ╮
│ define │ map f l │                      │
│        ╰         ╯                      │
│ ╭                                     ╮ │
│ │ match l                             │ │
│ │                                     │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮                       │ │
│ │ │ ┃  ┃ ┃  ┃ │                       │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯                       │ │
│ │ ╭ ╓      ╖                        ╮ │ │
│ │ │ ║ head ║                        │ │ │
│ │ │ ║ ____ ║                        │ │ │
│ │ │ ║      ║                        │ │ │
│ │ │ ║ tail ║                        │ │ │
│ │ │ ╙      ╜                        │ │ │
│ │ │ ╓ ╭        ╮ ╥ ╭            ╮ ╖ │ │ │
│ │ │ ║ │ f head │ ║ │ map f tail │ ║ │ │ │
╰ ╰ ╰ ╙ ╰        ╯ ╨|╰            ╯ ╜ ╯ ╯ ╯
")

(insert-character! #\newline)


(snapshot "
╭        ╭         ╮         ╮
│ define │ map f l │         │
│        ╰         ╯         │
│ ╭                        ╮ │
│ │ match l                │ │
│ │                        │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮          │ │
│ │ │ ┃  ┃ ┃  ┃ │          │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯          │ │
│ │ ╭ ╓      ╖           ╮ │ │
│ │ │ ║ head ║           │ │ │
│ │ │ ║ ____ ║           │ │ │
│ │ │ ║      ║           │ │ │
│ │ │ ║ tail ║           │ │ │
│ │ │ ╙      ╜           │ │ │
│ │ │ ╓ ╭        ╮ ╥   ╖ │ │ │
│ │ │ ║ │ f head │ ║   ║ │ │ │
│ │ │ ║ ╰        ╯ ╨   ║ │ │ │
│ │ │ ║ ╭            ╮ ║ │ │ │
│ │ │ ║ │ map f tail │ ║ │ │ │
╰ ╰ ╰ ╙|╰            ╯ ╜ ╯ ╯ ╯
")


(times 2 cursor-retreat!)
(insert-character! #\newline)

(snapshot "
╭        ╭         ╮         ╮
│ define │ map f l │         │
│        ╰         ╯         │
│ ╭                        ╮ │
│ │ match l                │ │
│ │                        │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮          │ │
│ │ │ ┃  ┃ ┃  ┃ │          │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯          │ │
│ │ ╭ ╓      ╖           ╮ │ │
│ │ │ ║ head ║           │ │ │
│ │ │ ║______║           │ │ │
│ │ │ ║      ║           │ │ │
│ │ │ ║ tail ║           │ │ │
│ │ │ ╙      ╜           │ │ │
│ │ │ ╓ ╭        ╮     ╖ │ │ │
│ │ │ ║ │ f head │     ║ │ │ │
│ │ │ ║>╰_______ ╯_____║ │ │ │
│ │ │ ║ ╭            ╮ ║ │ │ │
│ │ │ ║ │ map f tail │ ║ │ │ │
╰ ╰ ╰ ╙ ╰            ╯ ╜ ╯ ╯ ╯
")


(times 2 cursor-retreat!)
(delete-backward!)

(snapshot "
╭        ╭         ╮                  ╮
│ define │ map f l │                  │
│        ╰         ╯                  │
│ ╭                                 ╮ │
│ │ match l                         │ │
│ │                                 │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮                   │ │
│ │ │ ┃  ┃ ┃  ┃ │                   │ │
│ │ ╰ ┗  ┛ ┗  ┛ ╯                   │ │
│ │ ╭ ╓      ╖ ╓ ╭        ╮     ╖ ╮ │ │
│ │ │ ║ head ║ ║ │ f head │     ║ │ │ │
│ │ │ ║______║ ║_╰_______ ╯_____║ │ │ │
│ │ │ ║      ║ ║ ╭            ╮ ║ │ │ │
│ │ │ ║ tail ║ ║ │ map f tail │ ║ │ │ │
╰ ╰ ╰ ╙      ╜|╙ ╰            ╯ ╜ ╯ ╯ ╯
")

(move-cursor-up!)
(times 5 cursor-retreat!)

(snapshot "
╭        ╭         ╮                  ╮
│ define │ map f l │                  │
│        ╰         ╯                  │
│ ╭                                 ╮ │
│ │ match l                         │ │
│ │                                 │ │
│ │ ╭ ┏  ┓ ┏  ┓ ╮                   │ │
│ │ │ ┃  ┃ ┃  ┃ │                   │ │
│ │ ╰ ┗| ┛ ┗  ┛ ╯                   │ │
│ │ ╭ ╓      ╖ ╓ ╭        ╮     ╖ ╮ │ │
│ │ │ ║ head ║ ║ │ f head │     ║ │ │ │
│ │ │ ║______║ ║_╰_______ ╯_____║ │ │ │
│ │ │ ║      ║ ║ ╭            ╮ ║ │ │ │
│ │ │ ║ tail ║ ║ │ map f tail │ ║ │ │ │
╰ ╰ ╰ ╙      ╜ ╙ ╰            ╯ ╜ ╯ ╯ ╯
")


(times 4 insert-character! #\space)

(snapshot "
╭        ╭         ╮                  ╮
│ define │ map f l │                  │
│        ╰         ╯                  │
│ ╭                                 ╮ │
│ │ match l                         │ │
│ │                                 │ │
│ │ ╭ ┏      ┓ ┏  ┓ ╮               │ │
│ │ │ ┃      ┃ ┃  ┃ │               │ │
│ │ ╰ ┗    | ┛ ┗  ┛ ╯               │ │
│ │ ╭ ╓      ╖ ╓ ╭        ╮     ╖ ╮ │ │
│ │ │ ║ head ║ ║ │ f head │     ║ │ │ │
│ │ │ ║______║ ║_╰_______ ╯_____║ │ │ │
│ │ │ ║      ║ ║ ╭            ╮ ║ │ │ │
│ │ │ ║ tail ║ ║ │ map f tail │ ║ │ │ │
╰ ╰ ╰ ╙      ╜ ╙ ╰            ╯ ╜ ╯ ╯ ╯
")

(times 4 cursor-advance!)

(snapshot "
╭        ╭         ╮                  ╮
│ define │ map f l │                  │
│        ╰         ╯                  │
│ ╭                                 ╮ │
│ │ match l                         │ │
│ │                                 │ │
│ │ ╭ ┏      ┓ ┏  ┓ ╮               │ │
│ │ │ ┃      ┃ ┃  ┃ │               │ │
│ │ ╰ ┗      ┛ ┗| ┛ ╯               │ │
│ │ ╭ ╓      ╖ ╓ ╭        ╮     ╖ ╮ │ │
│ │ │ ║ head ║ ║ │ f head │     ║ │ │ │
│ │ │ ║______║ ║_╰_______ ╯_____║ │ │ │
│ │ │ ║      ║ ║ ╭            ╮ ║ │ │ │
│ │ │ ║ tail ║ ║ │ map f tail │ ║ │ │ │
╰ ╰ ╰ ╙      ╜ ╙ ╰            ╯ ╜ ╯ ╯ ╯
")


;; restore the original parameter values
(set! (the-document) original-document)
(set! (the-cursor) original-cursor)
