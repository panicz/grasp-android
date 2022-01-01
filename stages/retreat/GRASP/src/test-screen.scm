(import (define-interface)
        (define-type)
        (conversions)
        (screen)
        (text-screen)
        (parse)
        (draw)
        (examples)
        (assert))

;; this is what we're aiming at:

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

;|

(define parsed (call-with-input-string "\
(define (factorial n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))" parse))

(set! (current-screen) (TextScreen 39 12))

(define (screen-displays? s::string)::boolean
  (string=? ((current-screen):toString) s))

(draw! (head parsed))

(assert
 (screen-displays? &{&-
/        /             \              \
| define | factorial n |              |
|        \             /              |
|   /    /        \                 \ |
|   | if | <= n 0 |                 | |
|   |    \        /                 | |
|   |                               | |
|   |       1                       | |
|   |                               | |
|   |       /     /   /       \ \ \ | |
|   |       | * n | ! | - n 1 | | | | |
\   \       \     \   \       / / / / /
}))

(draw! (Over back: (head parsed)
             front: (Finger left: 8
                            top: 4)))

(assert
 (screen-displays? &{&-
/        /             \              \
| define | factorial n |              |
|        \             /              |
|   /    /        \                 \ |
|   | if_| <= n 0 |                 | |
|   |    \        /                 | |
|   |                               | |
|   |       1                       | |
|   |                               | |
|   |       /     /   /       \ \ \ | |
|   |       | * n | ! | - n 1 | | | | |
\   \       \     \   \       / / / / /
}))


#|
(e.g.
 (cursor at: (Finger left: 8 top: 4)
         in: parsed)
 ===> ...)
|#
