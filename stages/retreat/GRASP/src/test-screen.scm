(import
 (keyword-arguments)
 (cell-display-properties)
 (define-interface)
 (define-type)
 (conversions)
 (screen)
 (text-screen)
 (parse)
 (draw)
 (examples)
 (assert)
 (infix))

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

(display ((current-screen):toString))

#;(assert
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

(display ((current-screen):toString))

#;(assert
 (screen-displays? &{&-
/        /             \              \
| define | factorial n |              |
|        \             /              |
|   /    /        \                 \ |
|   | if@| <= n 0 |                 | |
|   |    \        /                 | |
|   |                               | |
|   |       1                       | |
|   |                               | |
|   |       /     /   /       \ \ \ | |
|   |       | * n | ! | - n 1 | | | | |
\   \       \     \   \       / / / / /
}))

(define horizontal-dotted (call-with-input-string "\
(head
.
tail)" parse))

(define vertical-dotted (call-with-input-string "\
(((a b)
(c d))  .  ((e f)
(g h)))" parse))

((current-screen):clear!)

(draw! (head horizontal-dotted))

(display ((current-screen):toString))

((current-screen):clear!)

(draw! (head vertical-dotted))

(display ((current-screen):toString))


#|
(e.g.
 (cursor at: (Finger left: 8 top: 4)
         in: (head parsed))
 ===> (5 2 0))
|#

;; Iteracja po wyrazeniach:
;; Schemat iteracji, ktorego uzywamy do rysowania sekwencji,
;; chcielibysmy rowniez wykorzystywac do przekazywania
;; zdarzen dotyku do poszczegolnych komponentow, a w szczegolnosci
;; do "wyciagania" podwyrazen, oraz do umieszczania kursora.
;;
;; Chcemy miec operacje, ktora
