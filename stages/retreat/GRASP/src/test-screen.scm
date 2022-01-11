(import
 (cell-display-properties)
 (define-interface)
 (define-type)
 (conversions)
 (screen)
 (text-screen)
 (combinators)
 (parse)
 (draw)
 (examples)
 (assert)
 (infix))

;; this is what we're aiming at:

&{
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


;; No dobra, do tej pory ("podejscie funkcyjne") bylismy w stanie
;; wyjasniac sobie kod za pomoca przykladow: dla takiego a takiego
;; wejscia otrzymamy takie a takie wyjscie.
;;
;; Wydaje sie jednak, ze w przypadku systemow interaktywnych
;; to podejscie jest niewystarczajace - ze raczej chcielibysmy
;; moc "opowiadac historie"

(define (grasped expression::string)::Screen
  (parameterize ((current-screen (TextScreen)))
    (let ((parsed (call-with-input-string expression parse)))
      (draw! (head parsed)))
    (current-screen)))
      
(define parsed (call-with-input-string "\
(define (factorial n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))" parse))

(set! (current-screen) (TextScreen))

(define (screen-displays? s::string)::boolean
  (string=? ((current-screen):toString) s))

(draw! (head parsed))

(display ((current-screen):toString))

#;(assert
 (screen-displays? &{
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
 (screen-displays? &{
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

;; No dobra, czyli tak:
;; 1. do interfejsu Screen dochodzi operacja
;;
;;      (cursor-at left::real top::real on: screen::Screen) -> cursor
;;
;;    ktora zwraca kursor wyrazenia dla danych wspolrzednych
;; 2. ponadto trzeba interfejs Screen odpowiednio rozbudowac,
;;    zeby wesprzec konstrukcje struktury, ktora bedzie nam
;;    odwzorowywac wspolrzedne w kursory
;; 3. chcielibysmy tez opracowac API do:
;;    - odnoszenia sie do elementu wskazywanego przez kursor
;;      (cursor-ref cursor expr) -> expr
;;    - wyciagania elementu
;;      (cursor-take! cursor expr) -> expr
;;    - umieszczania elementu
;;      (cursor-put! element cursor expr)
;;    - nawigowania w obrebie kursora:
;;      (cursor-next cursor expr) -> cursor
;;      (cursor-back cursor expr) -> cursor
;;
;; Co nie mniej wazne, chcemy 

