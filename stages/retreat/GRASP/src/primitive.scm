(import (define-property))
(import (cell-display-properties))
(import (screen))
(import (assert))
(import (infix))
(import (extent))
(import (cursor))
(import (match))
(import (examples))
(import (define-cache))
(import (for))

;; we override Pair with Object's default equality and hash functions
;; (TODO: patch the Kawa implementation of Cons)

;;  0 1 2 3 4 5 6
;; ( car  .  cdr )

;;  0 1 2  3 4  5  6 7 8  9  10
;; ( car cadr caddr  .  cdddr  )

;;  0 1 2
;; (  x  )


(define (part-at cursor::Cursor tile::Tile)::Tile
  (cond ((null? cursor)
         tile)
        ((pair? cursor)
         (let ((parent (part-at (tail cursor) tile)))
           (if parent
               (parent:part-at (head cursor) (null? (tail cursor)))
               parent)))
        (else
         #!null)))

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

(define (last-cell-index cell::pair #!optional (initial::int 2))::int
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

(define-cache (heads tail)
  (cache (head)
    (cons head tail)))

(define (recons head tail)::cons
  ((heads tail) head))

(define (parenthesized! proc/object+screen+cursor+context
			object #!key
                        (screen::Screen (current-screen))
                        (cursor::Cursor #f)
                        (context::Cursor '())
                        )::Extent
  (let* ((paren-width (screen:paren-width))
         (extent (with-translation screen (paren-width 0)
                   (as Extent (proc/object+screen+cursor+context
                               object screen cursor context)))))
    (screen:open-paren! extent:height 0 0)
    (screen:close-paren! extent:height (+ paren-width extent:width) 0)
    (Extent width: (+ paren-width extent:width paren-width)
            height: extent:height)))

;; Docelowo bedziemy musieli patchowac Kawe, chociazby po to,
;; zeby takie obiekty, jak Symbol czy FString (IString? MString?)
;; implementowaly Tile.
;;
;; Na razie jednak zrobmy tak, zeby nie uzywac tych typow
;; bezposrednio w edytorze, a zamiast tego korzystac sobie
;; z owijek

(define-simple-class Symbol (Tile gnu.mapping.SimpleSymbol)

  (name :: string)
  
  ((*init* source::string)
   (invoke-special gnu.mapping.SimpleSymbol (this) '*init*
                   ((source:toString):intern))
   (set! name source))
            
  ((draw! screen::Screen cursor::Cursor context::Cursor)::Extent
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

(define (draw! object::Tile #!key
               (screen::Screen (current-screen))
               (cursor::Cursor '())
               (context::Cursor '()))
  ::Extent
  (object:draw! screen cursor context))

(define (draw-sequence! elems #!key
                        (screen :: Screen (current-screen))
                        (cursor::Cursor '())
                        (context::Cursor '()))
  ::Extent
  (let ((max-width 0)
        (max-line-height (screen:min-line-height))
        (top 0)
        (left 0)
        (index 0))

    (define (skip-spaces! spaces::string)::void
      (for char in spaces
           (cond ((eq? char #\newline)
                  ;; powinnismy dodac wszystkie obiekty z tej linii
                  ;; do detekcji
                  (set! top (+ top max-line-height))
                  (set! left 0)
                  (set! max-line-height (screen:min-line-height)))
                 (else
                  (set! left (+ left 1))
                  (set! max-width (max max-width left)))))
      (set! index (+ index 1)))

    (define (advance! extent::Extent)::void
      (set! left (+ left extent:width))
      (set! max-line-height (max extent:height max-line-height))
      (set! max-width (max left max-width))
      (set! index (+ index 1)))

    (define (draw-empty-list! spaces::string)::Extent
      (parenthesized! (lambda (spaces screen)
                             (string-extent spaces))
                      spaces
                      screen: screen
                      cursor: cursor
                      context: context))

    (define (draw-head! pair)::Extent
      (let ((context (recons index context)))
        (with-translation screen (left top)
          (if (null? (head pair))
              (draw-empty-list! (null-head-space pair))
              (draw! (head pair)
                     screen: screen
                     cursor: (subcursor cursor context)
                     context: context)))))

    (define (should-the-bar-be-horizontal? dotted-pair)
      (and (string-index (post-head-space dotted-pair) (is _ eq? #\newline))
           (string-index (pre-tail-space dotted-pair) (is _ eq? #\newline))))

    (define (draw-dotted-tail! pair)::Extent
      (skip-spaces! (post-head-space pair))
      (cond ((should-the-bar-be-horizontal? pair)
             (let* ((bottom top)
                    (stored-index index))
               (skip-spaces! (pre-tail-space pair))
               (set! top (- top (screen:min-line-height)))
               (let ((context (recons index context)))
                 (advance! (with-translation screen (left top)
                             (if (null? (tail pair))
                                 (draw-empty-list! (null-tail-space pair))
                                 (draw! (tail pair)
                                        screen: screen
                                        cursor: (subcursor cursor context)
                                        context: context)))))
               (skip-spaces! (post-tail-space pair))
               (with-translation screen (0 bottom)
                 (screen:draw-horizontal-bar! max-width))))
            (else
             (let ((previous-left left)
                   (previous-top top))
               (advance! (Extent width: (screen:vertical-bar-width)
                                 height: 0))
               (skip-spaces! (pre-tail-space pair))
               (let ((context (recons cursor context)))
                 (advance! (with-translation screen (left top)
                             (if (null? (tail pair))
                                 (draw-empty-list! (null-tail-space pair))
                                 (draw! (tail pair)
                                        screen: screen
                                        cursor: (subcursor cursor context)
                                        context: context)))))
               (skip-spaces! (post-tail-space pair))
               (with-translation screen (previous-left previous-top)
                 (screen:draw-vertical-bar! max-line-height)))))
      (Extent width: max-width
              height: (+ top max-line-height)))

    (define (draw-next! pair)
      (skip-spaces! (pre-head-space pair))
      (advance! (draw-head! pair))
      (cond ((dotted? pair)
             (draw-dotted-tail! pair))

            ((pair? (tail pair))
             (skip-spaces! (post-head-space pair))
             (draw-next! (tail pair)))

            (else
             (skip-spaces! (post-head-space pair))
             (Extent width: max-width
                     height: (+ top max-line-height)))))
    
    (draw-next! elems)
    ))
