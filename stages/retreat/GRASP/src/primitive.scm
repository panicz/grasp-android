(import (define-property))
(import (cell-display-properties))
(import (screen))
(import (draw))
(import (assert))
(import (infix))
(import (rename (keyword-arguments) (define/kw define*)))
(import (extent))
(import (cursor))
(import (match))

;; we override Pair with Object's default equality and hash functions
;; (TODO: patch the Kawa implementation of Cons)

;;  0 1 2 3 4 5 6
;; ( car  .  cdr )

;;  0 1 2  3 4  5  6 7 8  9  10
;; ( car cadr caddr  .  cdddr  )

;;  0 1 2
;; (  x  )

(define (part-at cursor::Cursor tile::Tile)::Tile
  (match cursor
    ('() tile)
    (`(,index . ,indices)
     ((part-at indices tile):part-at index
      (null? indices)))
    (_ #!null)))

(define* (cell-subindices cell::pair initial::int := 3)::int
  (cond ((dotted? cell)
         (+ initial 4))
        ((pair? (tail cell))
         (cell-subindices (tail cell) (+ initial 2)))
        (else
         initial)))

(define-property (head-tail-separator cell)
  #!null)

(define (cell-index cell::pair index::int final::boolean)
  (assert (is index >= 0))
  (cond ((= index 0)
         (pre-head-space cell)) ;; trzeba jakos rzutowac do Tile
        ((= index 1)
         (let ((target (car cell)))
           (if (and (null? target) (not final))
               (null-head-space cell)
               target)))
        ((= index 2)
         (post-head-space cell)) ;; jak wyzej
        ((dotted? cell)
         (cond ((= index 3)
                (head-tail-separator cell))
               ((= index 4)
                (pre-tail-space cell)) ;; jakos rzutowac do Tile?
               ((= index 5)
                (let ((target (cdr cell)))
                  (if (and (null? target) (not final))
                      (null-tail-space cell)
                      target)))
               ((= index 6)
                (post-tail-space cell))))
        (else
         (cell-index (cdr cell) (- index 2) final))))

(define-simple-class cons (pair Tile)
  ((*init* a d)
   (invoke-special pair (this) '*init* a d))

  ((equals object)::boolean
   (eq? object (this)))

  ((hash-code)::int
   (java.lang.System:identity-hash-code (this)))

  ((draw! screen::Screen)::Extent
   (parenthesized! (lambda (object screen)
                     (draw-sequence! object on: screen))
                   (this) screen))

  ((part-at index::int final::boolean)::Tile
   (as Tile (cell-index (this) index final)))
  
  ((subindices)::int
   (cell-subindices (this)))
  )

;; Docelowo bedziemy musieli patchowac Kawe, chociazby po to,
;; zeby takie obiekty, jak Symbol czy FString (IString? MString?)
;; implementowaly Tile.
;;
;; Na razie jednak zrobmy tak, zeby nie uzywac tych typow
;; bezposrednio w edytorze, a zamiast tego korzystac sobie
;; z owijek

(define-simple-class Symbol (Tile)
  (name :: string)

  ((*init* source::string)
   (set! name source))

  ((toString)::String
   (name:toString))

  ((draw! screen::Screen)::Extent
   (screen:draw-atom! name))

  ((part-at index::int final::boolean)::Tile
   #!null)

  ((subindices)::int
   0)
  )

