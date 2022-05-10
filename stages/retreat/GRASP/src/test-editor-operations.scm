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
			      (document (current-document))
			      (cursor (current-cursor)))
  (parameterize ((current-screen (TextScreen)))
    (let* ((target (cursor-ref document cursor))
	   (screen ::TextScreen (current-screen)))
      (draw-sequence! (head document)
		      screen: screen
		      cursor: cursor)
      	  
      (screen:put! (if (is target instance? Space)
		       #\|
		       #\^)
		   (screen:remembered-top)
		   (screen:remembered-left))
      (screen:toString))))


;; "after" is a synonym for "begin" which assumes
;; that the last value is a test (i.e. a boolean expression)
(define-syntax-rule (after actions ... test)
  (begin actions ... test))


(define-syntax-rule (wish something ...)
  #;(something ...)
  (begin))


;; yields? is essentially a synonym for equal?, but when
;; there's only one argument given, it prints the value
;; to the screen and returns #t, which is useful for
;; capturing the result (it's a code-write-time convenience)
(define-syntax yields?
  (syntax-rules ()
    ((_ expression value)
     (equal? expression value))
    
    ((_ expression)
     (begin
       (display expression)
       (newline)
       #t))))

;; A few words about cursor: a document is implemented
;; as a pair whose first element is the actual contents
;; of the document, and whose second element is irrelevant
;; (but we set it to '()).
;;
;; A cursor is a path in a tree of nested sub-expressions,
;; but unlike, say, paths in UNIX, it is represented
;; from right to left (because this lets us avoid a lot
;; of consing, and also maximizes structural sharing via
;; hash-consing), so the last element of the list will
;; always be 1 (because that means the head of the top
;; list).
;;
;; The leftmost element, on the other hand, is a sub-unit
;; cursor position - it either points to a position within
;; a Space object, or a position within the Symbol object.
;; In case of lists, it can be #\[ or #\], meaning that it
;; points to either opening or closing paren of the
;; list object.
;;
;; For more information, see the `cursor.scm` file.



;; This document consists of a number of test cases, which
;; check for a different behaviors of the editor.

;; We save the original parameter values and restore it
;; after we're done.
(define original-document (current-document))
(define original-cursor (current-cursor))

(set! (current-document)
      ;; we're using (cons _ '()) rather than "list", because
      ;; Kawa's cons cells provide equal?-like equals method
      ;; which makes them work poorly with weak hash tables.
      ;;
      ;; We need to patch Kawa in order to resolve the problem,
      ;; but for now we just stick with what's in the repo.
      (cons (parse-string "(define (square x) (* x x))") '()))


;; First, let's describe our intuitions about the cursor:

(set! (current-cursor) (cursor #\[ 1 1))

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
^        ╰          ╯ ╰       ╯ ╯
")
      (yields? (cursor-ref)
	       '(define (square x) (* x x)))))

(cursor-advance!)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰ ^      ╰          ╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 0 1 1 1))
      (yields? (cursor-ref) 'define)))

(times 5 cursor-advance!)


(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰      ^ ╰          ╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 5 1 1 1))
      (yields? (cursor-ref) 'define)))

(cursor-advance!)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰       |╰          ╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 0 2 1 1))
      (yields? (cursor-ref) (Space fragments: '(1)))))

(cursor-advance!)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰        ^          ╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor #\[ 3 1 1))
      (yields? (cursor-ref) '(square x))))


(cursor-advance!)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰        ╰ ^        ╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 0 1 3 1 1))
      (yields? (cursor-ref) 'square)))

(times 5 cursor-advance!)


(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰        ╰      ^   ╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 5 1 3 1 1))
      (yields? (cursor-ref) 'square)))

(cursor-advance!)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰        ╰       |  ╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 0 2 3 1 1))
      (yields? (cursor-ref) (Space fragments: '(1)))))

(cursor-advance!)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰        ╰        ^ ╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 0 3 3 1 1))
      (yields? (cursor-ref) 'x)))


(cursor-advance!)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰        ╰         |╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 0 4 3 1 1))
      (yields? (cursor-ref) (Space fragments: '(0)))))

(cursor-advance!)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰        ╰          ^ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor #\] 3 1 1))
      (yields? (cursor-ref) '(square x))))

;; mind that if we now retreat the cursor, we won't get
;; to the previous position. 

(cursor-retreat!)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰        ╰         ^╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 1 3 3 1 1))
      (yields? (cursor-ref) 'x)))


;; Because of that, the editing operations
;; need to treat both those situations identically

(delete-backward!)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭         ╮ ╭       ╮ ╮
│ define │ square  │ │ * x x │ │
╰        ╰        |╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 1 2 3 1 1))
      (yields? (cursor-ref) (Space fragments: '(1)))))

(insert-character! #\x)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰        ╰         ^╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 1 3 3 1 1))
      (yields? (cursor-ref) 'x)))


;; A to jest troche niefajne, i warto sie zastanowic, czy
;; aby na pewno jest konieczne (tzn. czy by sie nie dalo
;; tak zrobic, zeby cofanie dzialalo po prostu jako odwrotnosc
;; ruchu do przodu)

(cursor-retreat!)

(e.g. ;; do zmiany
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰        ╰        | ╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 1 2 3 1 1))
      (yields? (cursor-ref) (Space fragments: '(1)))))

(cursor-advance!)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰        ╰        ^ ╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 0 3 3 1 1))
      (yields? (cursor-ref) 'x)))

(cursor-advance!)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰        ╰         |╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 0 4 3 1 1))
      (yields? (cursor-ref) (Space fragments: '(0)))))

;; Super. Przejdzmy teraz do wyspecyfikowywania
;; zachowania naszego edytora.

;; 1. WPISYWANIE ZNAKU

;; a. jezeli kursor znajduje sie ponad napisem (Caption),
;;    to po prostu dopisujemy znak zgodnie z normalnymi
;;    regulami pracy z napisami

;; NA RAZIE IGNORUJEMY, BO NIE MA NAPISOW!

;; b. jezeli znakiem jest spacja albo nowa linia, to
;;    - jezeli kursor znajduje sie na spacji, to powiekszamy
;;      te nasza spacje zgodnie z regulami

(insert-character! #\space)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭           ╮ ╭       ╮ ╮
│ define │ square x  │ │ * x x │ │
╰        ╰          |╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 1 4 3 1 1))
      (yields? (cursor-ref) (Space fragments: '(1)))))

(insert-character! #\newline)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭           ╮ ╭       ╮ ╮
│ define │ square x  │ │ * x x │ │
│        │           │ ╰       ╯ │
│        │           │           │
│        │           │           │
╰        ╰ |         ╯           ╯
")
      (yields? (current-cursor) (cursor 2 4 3 1 1))
      (yields? (cursor-ref) (Space fragments: '(1 0)))))

(delete-backward!)
(delete-backward!)


(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰        ╰         |╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 0 4 3 1 1))
      (yields? (cursor-ref) (Space fragments: '(0)))))



;;    - jezeli kursor znajduje sie na poczatku symbolu
;;      albo na nawiasie otwierajacym, to powiekszamy
;;      spacje poprzedzajaca ("na jej koncu")


;; trzeba popracowac nad ruchem kursora
(times 4 cursor-retreat!)
(times 2 cursor-advance!)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╮ ╭       ╮ ╮
│ define │ square x │ │ * x x │ │
╰        ╰        ^ ╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 0 3 3 1 1))
      (yields? (cursor-ref) 'x)))

(insert-character! #\space)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭           ╮ ╭       ╮ ╮
│ define │ square  x │ │ * x x │ │
╰        ╰         ^ ╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 0 3 3 1 1))
      (yields? (cursor-ref) 'x)))

;;    - jezeli kursor znajduje sie na koncu symbolu
;;      albo na nawiasie zamykajacym, to powiekszamy
;;      spacje nastepujaca ("na jej poczatku")


(set! (current-cursor) (cursor-next))

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭           ╮ ╭       ╮ ╮
│ define │ square  x │ │ * x x │ │
╰        ╰          ^╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 1 3 3 1 1))
      (yields? (cursor-ref) 'x)))

(insert-character! #\space)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭            ╮ ╭       ╮ ╮
│ define │ square  x  │ │ * x x │ │
╰        ╰          | ╯ ╰       ╯ ╯
")
      ;; TODO tutaj chyba powinien byc raczej
      ;; (cursor 1 4 3 1 1)
      (yields? (current-cursor) (cursor 0 4 3 1 1))
      (yields? (cursor-ref) (Space fragments: '(1)))))

;; TODO jeszcze nawias zamykajacy

;;    - jezeli kursor znajduje sie w srodku symbolu,
;;      to rozbijamy ten symbol na dwie czesci

(times 7 cursor-retreat!)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭            ╮ ╭       ╮ ╮
│ define │ square  x  │ │ * x x │ │
╰        ╰    ^       ╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 3 1 3 1 1))
      (yields? (cursor-ref) 'square)))

(insert-character! #\space)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭             ╮ ╭       ╮ ╮
│ define │ squ are  x  │ │ * x x │ │
╰        ╰    |        ╯ ╰       ╯ ╯
")
      ;; TODO: tutaj tez raczej wolelibysmy
      ;; (cursor 1 2 3 1 1), zeby backspace
      ;; dzialal poprawnie
      (yields? (current-cursor) (cursor 0 2 3 1 1))
      (yields? (cursor-ref) (Space fragments: '(1)))))

;; c. jezeli znakiem jest kropka albo | i jestesmy
;;    na spacji pomiedzy przedostatnim a ostatnim
;;    elementem listy, to konwertujemy te liste
;;    do postaci listy kropkowanej
;;    (chyba ze jest wcisniety klawisz ctrl:
;;    wtedy -- o ile jestesmy pomiedzy dwoma
;;    elementami albo za ostatnim elementem
;;    -- po prostu ustawiamy wlasciwosc 
;;    (dotted? <ostatnia-para>) na #true)


(times 5 cursor-advance!)
(insert-character! #\.)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭          ╷   ╮ ╭       ╮ ╮
│ define │ squ are  │ x │ │ * x x │ │
╰        ╰          ╵|  ╯ ╰       ╯ ╯
")
      (yields? (current-cursor) (cursor 0 6 3 1 1))
      (yields? (cursor-ref) (Space fragments: '(1)))))

(insert-character! #\newline)
(times 2 cursor-retreat!)
(insert-character! #\newline)

;; Tutaj tak naprawde bysmy woleli, zeby kursor
;; sie wyswietlal jakos tak:

;; ╭        ╭           ╮ ╭       ╮ ╮
;; │ define │ squ are   │ │ * x x │ │
;; │        │ |________ │ ╰       ╯ │
;; │        │           │           │
;; │        │ x         │           │
;; ╰        ╰           ╯           ╯


(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭           ╮ ╭       ╮ ╮
│ define │ squ are   │ │ * x x │ │
│        │ _________ │ ╰       ╯ │
│        │           │           │
│        │ x         │           │
╰        ╰ |         ╯           ╯
")
      (yields? (current-cursor) (cursor 3 4 3 1 1))
      (yields? (cursor-ref) (Space fragments: '(2 0)))))


;; tutaj powinnismy byc w stanie zaznaczyc "paleczke"
;; rozdzielajaca glowke od ogona. Ale nastepujaca zmiana
;; przesuwa nam kursor z post-head-space na pre-tail-space:
(cursor-advance!)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭           ╮ ╭       ╮ ╮
│ define │ squ are   │ │ * x x │ │
│        │ _________ │ ╰       ╯ │
│        │           │           │
│        │ x         │           │
╰        ╰ |         ╯           ╯
")
      (yields? (current-cursor) (cursor 0 6 3 1 1))
      (yields? (cursor-ref) (Space fragments: '(1 0)))))

;; d. jezeli znakiem jest #\[, #\( albo #\{, to
;;    - jezeli jestesmy na spacji, to rozdzielamy 
;;      te spacje nowa lista pusta

(times 9 cursor-retreat!)
(cursor-advance!)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭           ╮ ╭       ╮ ╮
│ define │ squ are   │ │ * x x │ │
│        │ ___|_____ │ ╰       ╯ │
│        │           │           │
│        │ x         │           │
╰        ╰           ╯           ╯
")
      (yields? (current-cursor) (cursor 0 2 3 1 1))
      (yields? (cursor-ref) (Space fragments: '(1)))))

(insert-character! #\[)

(e.g.
 (and (yields? (rendered-with-cursor) "
╭        ╭     ╭  ╮       ╮ ╭       ╮ ╮
│ define │ squ │  │ are   │ │ * x x │ │
│        │ ____╰|_╯______ │ ╰       ╯ │
│        │                │           │
│        │ x              │           │
╰        ╰                ╯           ╯
")
      (yields? (current-cursor) (cursor 0 0 3 3 1 1))
      (yields? (cursor-ref) (Space fragments: '(0)))))



;;    - jezeli jestesmy na symbolu, to owijamy
;;      ten symbol w liste
;;    - jezeli jestesmy na nawiasie zamykajacym,
;;      to idziemy do odpowiadajacego nawiasu
;;      otwierajacego
;;    - jezeli jestesmy na nawiasie otwierajacym,
;;      to owijamy dane wyrazenie w liste



#|
(cursor-advance!)

(e.g.
 (and (yields? (rendered-with-cursor) )
      (yields? (current-cursor) )
      (yields? (cursor-ref) )))

;; kawa --no-warn-unreachable -f test-editor-operations.scm
|#



;; restore the original parameter values
(set! (current-document) original-document)
(set! (current-cursor) original-cursor)
