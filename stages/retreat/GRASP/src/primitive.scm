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
(import (examples))

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
     (and-let* ((parent (part-at indices tile)))
       (parent:part-at index (null? indices))))
    (_ #f)))

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

(define* (last-cell-index cell::pair initial::int := 2)::int
  (cond ((dotted? cell)
         (+ initial 4))
        ((pair? (tail cell))
         (last-cell-index (tail cell) (+ initial 2)))
        (else
         initial)))

(define-property (head-tail-separator cell)
  #!null)

(define-simple-class cons (pair Tile)
  ((*init* a d)
   (invoke-special pair (this) '*init* a d))

  ((equals object)::boolean
   (eq? object (this)))

  ((hash-code)::int
   (java.lang.System:identity-hash-code (this)))

  ((draw! screen::Screen cursor::Cursor context::Cursor)::Extent
   (parenthesized! (lambda (object screen cursor context)
                     (draw-sequence! object
                                     screen: screen
                                     cursor: cursor
                                     context: context))
                   (this)
                   screen: screen
                   cursor: cursor
                   context: context))
  
  ((part-at index::Index final::boolean)::Tile
   (if (or (eq? index #\() (eq? index #\)))
       (this)
       (as Tile (cell-index (this) (as int index) final))))
  ((first-index)::Index
   #\()
   
  ((last-index)::Index
   #\))

  ((next-index index::Index)::Index
   (if (eq? index #\()
       0
       (if (and (integer? index)
                (is index < (last-cell-index (this))))
           (+ index 1)
           #\))))
  
  ((previous-index index::Index)::Index
   (if (and (integer? index)
            (is index > 0))
       (- index 1)
       #\())
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

  ((draw! screen::Screen cursor::Cursor context::Cursor)::Extent
   ;;(display "atom ")(display name)(display " is at ")(display context)(newline)
   (screen:draw-atom! name))

  ((part-at index::Index final::boolean)::Tile
   #!null
   )

  ((first-index)::Index
   0)

  ((last-index)::Index
   (max 0 (string-length name)))
  
  ((next-index index::Index)::Index
   (let ((last::int (as int (last-index))))
     (if (is index < last)
         (+ index 1)
         last)))
   
  ((previous-index index::Index)::Index
   (if (is index > 0)
       (- index 1)
       0))
  )

