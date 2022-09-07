(import
  (define-interface)
  (define-type)
  (define-object)
  (conversions)
  (extent)
  (indexable)
  (painter)
  (space)
  (cursor)
  (primitive)
  (symbol)
  (combinators)
  (text-painter)
  (parse)
  (examples)
  (assert)
  (infix)
  (match)
  (functions)
  (print)
  )

#|
(define parsed (with-input-from-string "\
(#;(a (b c) d) #|efg|# ;hij
 define (! n #|int|#) ; -> int
  (if #;(<= n 1) (is n <= 1)
      1 ; base case
      (* n (! (- n 1)))))" parse-document))
|#

(define document (with-input-from-string "\
(define (! n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))

(e.g. (! 5) ===> 120)
" parse-document))

(set! (the-document) document)

;;(show parsed)


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

(define (grasped expression::string)::Painter
  (parameterize ((the-painter (TextPainter)))
    (let ((parsed (call-with-input-string expression parse)))
      (draw! (head parsed)))
    (as Painter (the-painter))))

(set! (the-painter) (TextPainter))

(define (painter-displays? s::string)::boolean
  (string=? ((the-painter):toString) s))

(draw-document! document)

(parameterize ((evaluating? #t))
  (e.g. (the-expression at: '(1 1 1)) ===> define)
  (e.g. (the-expression at: '(1 3 1 1)) ===> !)
  (e.g. (the-expression at: '(3 3 1 1)) ===> n)
  (e.g. (the-expression at: '(1 5 1 1)) ===> if)
  (e.g. (the-expression at: '(1 3 5 1 1)) ===> <=)
  (e.g. (the-expression at: '(5 5 1 1)) ===> 1)
  (e.g. (the-expression at: '(1 7 5 1 1)) ===> *)
  (e.g. (the-expression at: '(3 7 5 1 1)) ===> n)
  (e.g. (the-expression at: '(1 5 7 5 1 1)) ===> !)
  (e.g. (the-expression at: '(1 3 5 7 5 1 1)) ===> -)
  (e.g. (the-expression at: '(3 3 5 7 5 1 1)) ===> n)
  (e.g. (the-expression at: '(5 3 5 7 5 1 1)) ===> 1)
  )

(e.g. (is '(1 1 1) cursor< '(1 3 1 1)))
(e.g. (isnt '(1 3 1 1) cursor< '(1 1 1)))
(e.g. (is '(1 3 1 1) cursor< '(3 3 1 1)))
(e.g. (isnt '(3 3 1 1) cursor< '(1 3 1 1)))
(e.g. (is '(3 3 1 1) cursor< '(1 5 1 1)))
(e.g. (isnt '(1 5 1 1) cursor< '(3 3 1 1)))	

(DUMP (the-expression at: '(#\[ 3 1 1)))

(define x 18)
(define y 10)

(assert
 (painter-displays? "
╭        ╭     ╮                      ╮
│ define │ ! n │                      │
│        ╰     ╯                      │
│   ╭    ╭        ╮                 ╮ │
│   │ if │ <= n 0 │                 │ │
│   │    ╰        ╯                 │ │
│   │                               │ │
│   │       1                       │ │
│   │                               │ │
│   │       ╭     ╭   ╭       ╮ ╮ ╮ │ │
│   │       │ * n │ ! │ - n 1 │ │ │ │ │
╰   ╰       ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
                                       
                                       
                                       
╭      ╭     ╮          ╮              
│ e.g. │ ! 5 │ ===> 120 │              
╰      ╰     ╯          ╯              
"))

(invoke (as TextPainter (the-painter))
	'put! #\⊙ y x)

(display ((the-painter):toString))


;; w sobote chcemy zrobic commit z komputera,
;; natomiast po nim mielibysmy nastepujace prace:
;; - wdrozyc ten design z cursor-under*
;; - (the) document jako argument domyslny w cursor-under
;; 
(define everything-ready? #false)

#|
              11111111112222222222333333333
    012345678901234567890123456789012345678
  0 ╭        ╭     ╮                      ╮
  1 │ define │ ! n │                      │
  2 │        ╰     ╯                      │
  3 │   ╭    ╭        ╮                 ╮ │
  4 │   │ if │ <= n 0 │                 │ │
  5 │   │    ╰        ╯                 │ │
  6 │   │                               │ │
  7 │   │       1                       │ │
  8 │   │                               │ │
  9 │   │       ╭     ╭   ╭       ╮ ╮ ╮ │ │
 10 │   │       │ * n │ ! │ - n 1 │ │ │ │ │
 11 ╰   ╰       ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
 12                                        
 13                                        
 14                                        
 15 ╭      ╭     ╮          ╮              
 16 │ e.g. │ ! 5 │ ===> 120 │              
 17 ╰      ╰     ╯          ╯              

|#

#;(when everything-ready?
  (e.g. (cursor-under 0 0) ===> (#\[ 1 1)) ; (define ...)
  (e.g. (cursor-under 0 11) ===> (#\[ 1 1)) ; (define ...)
  (e.g. (cursor-under 0 38) ===> (#\] 1 1)) ; (define ...)
  (e.g. (cursor-under 11 38) ===> (#\] 1 1)) ; (define ...)
  (e.g. (cursor-under 2 1) ===> (0 1 1 1)) ; define
  (e.g. (cursor-under 7 1) ===> (5 1 1 1)) ; define
  (e.g. (cursor-under 9 1) ===> (#\[ 3 1 1)) ; (! n)
  (e.g. (cursor-under 11 1) ===> (0 1 3 1 1)) ; !
  (e.g. (cursor-under 12 1) ===> (0 2 3 1 1)) ; [Space (1)]
  (e.g. (cursor-under 15 1) ===> (#\] 3 1 1)) ; (! n)
  (e.g. (cursor-under 4 3) ===> (#\[ 5 1 1)) ; (if ...)
  (e.g. (cursor-under 4 11) ===> (#\[ 5 1 1)) ; (if ...)
  (e.g. (cursor-under 6 4) ===> (0 1 5 1 1)) ; if
  (e.g. (cursor-under 9 4) ===> (#\[ 3 5 1 1)) ; (<= n 0)
  (e.g. (cursor-under 18 4) ===> (#\] 3 5 1 1)) ; (<= n 0)
  (e.g. (cursor-under 12 7) ===> (0 5 5 1 1)) ; 1
  (e.g. (cursor-under 12 10) ===> (#\[ 7 5 1 1)) ; (* n ...)
  (e.g. (cursor-under 0 16) ===> (#\[ 3 1)) ; (e.g. ...)
  (e.g. (cursor-under 24 17) ===> (#\] 3 1)) ; (e.g. ...)
  (e.g. (cursor-under 7 16) ===> (#\[ 3 3 1)) ; (! 5)
  )
 
(define horizontal-dotted (call-with-input-string "\
(head
.
tail)" parse))

(define vertical-dotted (call-with-input-string "\
(((a b)
(c d))  .  ((e f)
(g h)))" parse))

(invoke (the-painter) 'clear!)

(draw! (head horizontal-dotted))

(display (invoke (the-painter) 'toString))

(invoke (the-painter) 'clear!)

(draw! (head vertical-dotted))

(display (invoke (the-painter) 'toString))

(define empties (call-with-input-string "\
((() . ())
    (   )
.
     ( ))" parse))

((the-painter):clear!)

(draw! (head empties))

(display (invoke (the-painter) 'toString))


((the-painter):clear!)

((the-painter):draw-rounded-rectangle! 15 4)

(display (invoke (the-painter) 'toString))



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
;; 1. do interfejsu Painter dochodzi operacja
;;
;;      (cursor-at left::real top::real on: painter::Painter) -> cursor
;;
;;    ktora zwraca kursor wyrazenia dla danych wspolrzednych
;; 2. ponadto trzeba interfejs Painter odpowiednio rozbudowac,
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


