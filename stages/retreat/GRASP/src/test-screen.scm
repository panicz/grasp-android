(import
  (define-interface)
  (define-type)
  (define-object)
  (conversions)
  (extent)
  (indexable)
  (screen)
  (space)
  (cursor)
  (tile)
  (primitive)
  (symbol)
  (combinators)
  (text-screen)
  (parse)
  (examples)
  (assert)
  (infix)
  (match)
  (functions)
  )

(define parsed (with-input-from-string "\
(#;(a (b c) d) #|efg|# ;hij
 define (factorial n #|int|#) ; -> int
  (if #;(<= n 1) (is n <= 1)
      1 ; base case
      (* n (! (- n 1)))))" parse-document))


(define parsed (parse-string "\
(define (factorial n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))"))

(show parsed)

(define cur '())

(define (ccf)
  (set! cur (cursor-climb-front cur parsed))
  cur)

(define (ccb)
  (set! cur (cursor-climb-back cur parsed))
  cur)

(define (cn)
  (set! cur (cursor-next cur parsed))
  cur)

(define (cb)
  (set! cur (cursor-back cur parsed))
  cur)

(define (at)
  (cursor-ref parsed cur))


;;(set! (cadar parsed) '())


(e.g.
 (let ((document ::Tile (as Tile (parse-string "
(define (! n)
  (if (<= n 0)
    1
   (* n (! (- n 1)))))

(e.g. (! 5) ===> 120)
"))))
   ;;(show (part-at '(5 5 1)))   
   (and (equal? (cursor-ref document '(1 1))
		'define)
	(equal? (cursor-ref document '(1 3 1))
		'!)
	(equal? (cursor-ref document '(3 3 1))
	     'n)
	(equal? (cursor-ref document '(1 5 1))
	     'if)
	(equal? (cursor-ref document '(1 3 5 1))
		'<=)
#|
	(equal? (cursor-ref document '(5 5 1))
		1)
|#
	(equal? (cursor-ref document '(1 7 5 1))
		'*)
	(equal? (cursor-ref document '(3 7 5 1))
		'n)

	(equal? (cursor-ref document '(1 5 7 5 1))
		'!)
	(equal? (cursor-ref document '(1 3 5 7 5 1))
		'-)
	(equal? (cursor-ref document '(3 3 5 7 5 1))
		'n)
	(cursor< '(1 1) '(1 3 1) document)
	(not (cursor< '(1 3 1) '(1 1) document))
	(cursor< '(1 3 1) '(3 3 1) document)
	(not (cursor< '(3 3 1) '(1 3 1) document))
	(cursor< '(3 3 1) '(1 5 1) document)
	(not (cursor< '(1 5 1) '(3 3 1) document))
	
	
	#|
	(equal? (cursor-ref document '(5 3 5 7 5 1))
		1)
|#
	)))



;; this is what we're aiming for:


&{
╭                         ╮
│                ╭─────╮  │
│ ╭───────╮     ╭╯     ╰╮ │
│ │       │     │   B   │ │
│ │   A   │----⯈╰╮     ╭╯ │
│ │       │      ╰─────╯  │
│ ╰───────╯       ╱       │
│        ⯈       ╱        │
│         ╲     ⯈         │
│          ╭───╮          │
│          │ C │          │
│          ╰───╯          │
╰                         ╯
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
  (parameterize ((the-screen (TextScreen)))
    (let ((parsed (call-with-input-string expression parse)))
      (draw! (head parsed)))
    (the-screen)))

(set! (the-screen) (TextScreen))

(define (screen-displays? s::string)::boolean
  (string=? ((the-screen):toString) s))

;(draw! (head parsed))

(display ((the-screen):toString))

#;(assert
 (screen-displays? &{
╭        ╭             ╮              ╮
│ define │ factorial n │              │
│        ╰             ╯              │
│   ╭    ╭        ╮                 ╮ │
│   │ if │ <= n 0 │                 │ │
│   │    ╰        ╯                 │ │
│   │                               │ │
│   │       1                       │ │
│   │                               │ │
│   │       ╭     ╭   ╭       ╮ ╮ ╮ │ │
│   │       │ * n │ ! │ - n 1 │ │ │ │ │
╰   ╰       ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
}))


(define horizontal-dotted (call-with-input-string "\
(head
.
tail)" parse))

(define vertical-dotted (call-with-input-string "\
(((a b)
(c d))  .  ((e f)
(g h)))" parse))

(invoke (the-screen) 'clear!)

(draw! (head horizontal-dotted))

(display (invoke (the-screen) 'toString))

(invoke (the-screen) 'clear!)

(draw! (head vertical-dotted))

(display (invoke (the-screen) 'toString))

(define empties (call-with-input-string "\
((() . ())
    (   )
.
     ( ))" parse))

((the-screen):clear!)

(draw! (head empties))

(display (invoke (the-screen) 'toString))


((the-screen):clear!)

((the-screen):draw-rounded-rectangle! 15 4)

(display (invoke (the-screen) 'toString))



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


