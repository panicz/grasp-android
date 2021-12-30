(import (define-interface)
        (define-type)
        (conversions)
        (screen)
        (text-screen)
        (parse)
        (draw)
        (examples))

(e.g.
 (let ((parsed (call-with-input-string "\
(define (factorial n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))" parse))
       (screen (TextScreen 41 12)))
    (draw! parsed screen)
    (screen:toString))
  ===> &{&-
/ /        /             \              \
| | define | factorial n |              |
| |        \             /              |
| |   /    /        \                 \ |
| |   | if | <= n 0 |                 | |
| |   |    \        /                 | |
| |   |                               | |
| |   |       1                       | |
| |   |                               | |
| |   |       /     /   /       \ \ \ | |
| |   |       | * n | ! | - n 1 | | | | |
\ \   \       \     \   \       / / / / /
})
