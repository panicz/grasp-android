(begin
  (import (define-syntax-rule))
  (import (assert))
  (import (conversions))
  (import (define-interface))
  (import (define-type))
  (import (examples))
  (import (infix))
  (import (match))
  (import (for))
  )
  
(define-interface Canvas ()
  (clear):: void
  )

(define-simple-class TextCanvas (Canvas)
  (shift-left :: int)
  (shift-top :: int)
  (width :: int)
  (height :: int)
  (data :: char[] access: 'private)
  ((*init* w h)
   (set! width w)
   (set! height h)
   (set! data (char[] length: (* w h)))
   (clear))
	 
  ((clear)::void
   (for line from 0 below height
	(for row from 0 below width
	     (set! (data (+ (* line width) row))
	       #\space))))
  
  ((toString)::String
   (with-output-to-string
     (lambda ()
       (for line from 0 below height
	    (for row from 0 below width
		 (write-char (data (+ (* line width) row))))
	    (write-char #\newline)))))
  )
  
(define c (TextCanvas 32 12))

(e.g.
 (let ((parsed (with-input-from-string "\
(define (factorial n)
  (if (<= n 0)
      1
    (* n (! (- n 1)))))" parse))
       (screen (TextScreen width: 28
			   height: 12)))
   (screen:draw! parsed)
   (screen:toString))
 ;; Kawa supports these weird string literals
 ;; which allow us not to escape backslashes
 ===> &{&-
/        /             \   \
| define | factorial n |   |
|        \             /   |
| /     /        \       \ |
| | if  | <= n 0 |       | |
| |     \        /       | |
| |                      | |
| |    1                 | |
| |                      | |
| |  /     /   /       \ | |
| |  | * n | ! | - n 1 | | |
\ \  \     \   \       / / /})


&{&-
/      \
| head |
| ---- |
| tail |
\      /}

&{&-
/       \
| a | b |
\       /}
;|


(define-extension (Graph . nodes)
  ;; nodes are displayed as circles
  ;; (in the regular textual order)
  ;; whereas arrows are rendered over
  ;; the graph, joining centers of the nodes
  ;; (minus their radii) with overlaying
  ;; arrows
  (let ((nodes (textually-ordered
		(circle label: node)
		for `(,node . ,neighbours) in nodes)))
    (fold-left (lambda (nodes `(,node . ,neighbours))
		 (fold-left (lambda (nodes neighbour)
			      (let* ((start (position of: node
						      in: nodes))
				     (end (position of: neighbour
						    in: nodes))
				     (vector (- end start))
				     (direction (normalized vector)))
				     
				(over
				 (arrow from: (+ start
						 (* node:radius
						    direction))
					to: (- end
					       (* neighbour:radius
						  direction)))
				 nodes)))
			    nodes
			    neighbours))
	       nodes
	       nodes)))


&{&-
/       \
| a b c |
| ----- |
| d     |
\       /
  }



&{&-
/                       \
|                 ___   |
|   ___          /   \  |
|  /   \   __-->|  B  | |
| |  A  |--      \___/  |
|  \___/\        /      |
|        \      |       |
|         \     |       |
|          _|___v       |
|           /   \       |
|          |  C  |      |
|           \___/       |
\                       /
}

&{&-
/         /             \     \
: define  | factorial n |     :
:         \             /     :
: /     /         \         \ :
: : if  : <= n@ 0 :         : :
: :     \         /         : :
: :                         : :
: :    1                    : :
: :                         : :
: :  /        /   /       \ : :
: :  |  *  n  | ! | - n 1 | : :
\ \  \        \   \       / / /
}

