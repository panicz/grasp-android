(import (define-syntax-rule))
(import (define-interface))
(import (define-type))
(import (define-property))
(import (cell-display-properties))
(import (assert))
(import (infix))
(import (extent))
(import (cursor))
(import (match))
(import (examples))
(import (define-cache))
(import (for))
;;(import (rename (keyword-arguments) (define/kw define*)))


;; Each tile can choose whatever it pleases to be its index
;; (except #!null, for the reason explained below)
;; For built-in types (boxes, combinators, atoms) indices are
;; typically either integers or characters or symbols.
;;
;; The special value #!null means the absence of an index

(define-alias Index java.lang.Object)

(define-interface Screen ()
  (paren-width)::real
  (min-line-height)::real
  (vertical-bar-width)::real

  (clear!)::void
  (translate! x::real y::real)::void
  (draw-string! s::string left::real top::real)::Extent
  (draw-text! s::string left::real top::real)::real
  (draw-atom! text::string)::Extent
  (draw-finger! left::real top::real index::byte)::Extent
  (draw-horizontal-bar! width::real)::void
  (draw-vertical-bar! height::real)::void
  (open-paren! height::real left::real top::real)::void
  (close-paren! height::real left::real top::real)::void

  ;; do usuniecia:
  (cursor-left)::real
  (cursor-top)::real

  (cursor-next!)::void
  (cursor-back!)::void
  (cursor-up!)::void
  (cursor-down!)::void
  
  ;;(end-line! line-height::real)::void
  ;;(cursor-at left::real top::real)::Cursor
  )

(define-constant final-part?::parameter[boolean]
  (make-parameter #f))

(define-interface Indexable ()
  (has-children?)::boolean
  
  (part-at index::Index)::Indexable
  
  (first-index)::Index
  (last-index)::Index
  
  (next-index index::Index)::Index
  (previous-index index::Index)::Index
)

(define (part-at index::Index object)
  (cond ((instance? object Indexable)
	 (invoke (as Indexable object) 'part-at index))

	(else
	 (error "Don't know how to extract "index" from "object))))
#|
(define-interface SpaceType ())

(define-type (NonBreakingSpace width: real)
  implementing SpaceType)

(define-type (BreakingSpace coda: real
			    line-width: real
			    empty-lines: real
			    indentation: real)
  implementing SpaceType)

(define-type (Space type: SpaceType)
  implementing Indexable
  with
  ((has-children?)::boolean #t)
  ((part-at index::Index)::Indexable (this))
  ((first-index)::Index 0)
  ((last-index)::Index
   (match type
     ((NonBreakingSpace width: width)
      width)
     ((BreakingSpace coda: coda
		     line-width: line-width
		     empty-lines: empty-lines
		     indentation: indentation)
      (+ coda indentation (* line-width empty-lines)))))
  
  ((next-index index::Index)::Index
   (min (+ index 1) (last-index)))
  
  ((previous-index index::Index)::Index
   (max (- index 1) 0)))

|#

(define-interface Tile (Indexable)
  (draw! screen::Screen cursor::Cursor context::Cursor)::Extent
  )

(define (draw! object #!key
               (screen::Screen (current-screen))
               (cursor::Cursor '())
               (context::Cursor '()))
  ::Extent
  (cond ((instance? object Tile)
	 (invoke (as Tile object) 'draw! screen cursor context))

	(else
	 (error "Don't know how to draw "object))))


(define-type (Finger left: real := 0
                     top: real := 0
                     index: byte)
  implementing Tile
  with
  ((draw! screen::Screen cursor::Cursor context::Cursor)::Extent
   (let ((finger (screen:draw-finger! left top index)))
     (Extent width: (+ left finger:width)
             height: (+ top finger:height))))

  ((has-children?)::boolean #f)

  ((part-at index::Index)::Indexable
   (this))
  
  ((first-index)::Index
   #!null)
  
  ((last-index)::Index
   #!null)
  
  ((next-index index::Index)::Index
   #!null)
  
  ((previous-index index::Index)::Index
   #!null)
  
  )

(define-simple-class NullScreen (Screen)
  ((paren-width)::real 0)

  ((min-line-height)::real 0)
  
  ((vertical-bar-width)::real 0)
 
  ((clear!)::void
   (values))
  
  ((translate! x::real y::real)::void
   (values))
  
  ((draw-string! s::string left::real top::real)::Extent
   (string-extent s))
  
  ((draw-text! s::string left::real top::real)::real
   (string-length s))
  
  ((draw-atom! text::string)::Extent
   (Extent width: (string-length text) height: 1))

  ((draw-finger! left::real top::real index::byte)::Extent
   (Extent width: 1 height: 1))

  ((draw-horizontal-bar! width::real)::void
   (values))
  
  ((draw-vertical-bar! height::real)::void
   (values))
  
  ((open-paren! height::real left::real top::real)::void
   (values))
  
  ((close-paren! height::real left::real top::real)::void
   (values))

  ((cursor-left)::real
   0)

  ((cursor-top)::real
   0)

  ((cursor-next!)::void
   (values))
  
  ((cursor-back!)::void
   (values))
  
  ((cursor-up!)::void
   (values))
  
  ((cursor-down!)::void
   (values))
  
  )

(define-constant current-screen::parameter[Screen]
  (make-parameter (NullScreen)))

(define-syntax-rule (with-translation screen (x y) . actions)
  (let ((x! x)
        (y! y))
    (screen:translate! x! y!)
    (let ((result (begin . actions)))
      (screen:translate! (- x!) (- y!))
      result)))


;; we override Pair with Object's default equality and hash functions
;; (TODO: patch the Kawa implementation of Cons)

;;  0 1 2 3 4 5 6
;; ( car  .  cdr )

;;  0 1 2  3 4  5  6 7 8  9  10
;; ( car cadr caddr  .  cdddr  )

;;  0 1 2
;; (  x  )

(define (cell-index cell::pair index::int)
  (assert (is index >= 0))
  (cond ((= index 0)
         (pre-head-space cell)) ;; trzeba jakos rzutowac do Tile
        ((= index 1)
         (let ((target (car cell)))
           (if (and (null? target) (not (final-part?)))
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
                  (if (and (null? target) (not (final-part?)))
                      (null-tail-space cell)
                      target)))
               ((= index 6)
                (post-tail-space cell))))
        (else
         (cell-index (cdr cell) (- index 2)))))

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

  ((has-children?)::boolean #t)
  
  ((part-at index::Index)::Indexable
   (if (or (eq? index #\() (eq? index #\)))
       (this)
       (as Tile (cell-index (this) (as int index)))))
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

  ((has-children?)::boolean #f)
  
  ((part-at index::Index)::Indexable
   (this))

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
    
    (define (draw-head! pair::cons)::Extent
      (let ((context (recons index context)))
        (with-translation screen (left top)
          (if (null? (head pair))
              (draw-empty-list! (null-head-space pair))
              (draw! (head pair)
                     screen: screen
                     cursor: (subcursor cursor context)
                     context: context)))))

    (define (should-the-bar-be-horizontal? dotted-pair::cons)::boolean
      (and (string-any (is _ eq? #\newline)
		       (post-head-space dotted-pair))
           (string-any (is _ eq? #\newline)
		       (pre-tail-space dotted-pair))))

    (define (draw-dotted-tail! pair::cons)::Extent
      (skip-spaces! (post-head-space pair))
      (cond ((should-the-bar-be-horizontal? pair)
             (let* ((bottom top)
                    (stored-index index))
               (skip-spaces! (pre-tail-space pair))
               (set! top (- top (screen:min-line-height)))
               (let ((context (recons index context)))
                 (advance! (with-translation screen (left top)
                             (if (null? (tail pair))
                                 (draw-empty-list! (null-tail-space
						    pair))
                                 (draw! (tail pair)
                                        screen: screen
                                        cursor: (subcursor cursor
							   context)
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
                                 (draw-empty-list! (null-tail-space
						    pair))
                                 (draw! (tail pair)
                                        screen: screen
                                        cursor: (subcursor cursor
							   context)
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


(define (cursor-ref tile::Indexable cursor::Cursor)::Indexable
  (cond ((null? cursor)
         tile)
        ((pair? cursor)
         (let ((parent (cursor-ref tile (tail cursor)))
	       (final? (null? (tail cursor))))
           (if parent
	       (if (isnt final? eq? (final-part?))
		   (parameterize ((final-part? final?))
		     (part-at (head cursor) parent))
		   ;; don't need to change the final-part?
		   ;; parameter:
		   (part-at (head cursor) parent))
               parent)))
        (else
         #!null)))

;; RZM37UHSPY5Z

;; no dobra, jak powinien dzialac cursor-next?
;; 1. bierzemy element spod kursora i pytamy go o pierwszy
;; pod-indeks
;; 2. jezeli nie ma takowego, to pytamy rodzica o kolejmy 
;; pod-indeks wzgledem naszego
;; 3. i teraz uwaga: jezeli rodzic nie ma kolejnego
;; indeksu, to powinien zapytac dziadka itd. (az dojdziemy
;; do Adama/Ewy)

(define (cursor-next cursor::Cursor document::Tile)::Cursor
  (match cursor
    (`(,head . ,tail)
     (let* ((parent (cursor-ref document tail))
	    (next (parent:next-index head)))
       (if (equal? head next)
	   (cursor-next tail document)
	   (recons next tail))))
    (_
     cursor)))

(define (cursor-climb-front cursor::Cursor document::Indexable)::Cursor

  (define (climb-front cursor::Cursor target::Indexable)::Cursor
    (if (target:has-children?)
	(let* ((index (target:first-index))
	       (child (part-at index target)))
	  (if (eq? child target)
	      (recons index cursor)
	      (climb-front (recons index cursor)
			   child)))
	cursor))
    
  (climb-front cursor (cursor-ref document cursor)))

(define (cursor-back cursor::Cursor document::Indexable)::Cursor
  (match cursor
    (`(,head . ,tail)
     (let* ((parent (cursor-ref document tail))
	    (previous (parent:previous-index head)))
       (if (equal? head previous)
	   (cursor-back tail document)
	   (recons previous tail))))
    (_
     cursor)))


(define (cursor-climb-back cursor::Cursor document::Indexable)::Cursor

  (define (climb-back cursor::Cursor target::Indexable)::Cursor
    (if (target:has-children?)
	(let* ((index (target:last-index))
	       (child (part-at index target)))
	  (if (eq? child target)
	      (recons index cursor)
	      (climb-back (recons index cursor)
			  child)))
	cursor))
    
  (climb-back cursor (cursor-ref document cursor)))


;;   
;; (   (   a   b   )   )
;; ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^
;; 1 1 1 1 1 1 1 1 1 1 1
;; ( 0 1 1 1 1 1 1 1 2 )
;;     ( 0 1 2 3 4 )

#|
(e.g.
 (let ((context (head (parse-string
                       "  (   (   a   b   )   )"
                       ;; ^(#\()               ;
                       ;;  ^(0 0)              ;
                       ;;   ^(1 0)             ;
                       ;;    ^(2 0)            ;
                       ;;     ^(#\( 1)         ;
                       ;;      ^(0 0 1)        ;
                       ;;       ^(1 0 1)       ;
                       ;;        ^(2 0 1)      ;
                       ;;         ^(0 1 1)     ;
                       ;;          ^(0 2 1)    ;
                       ;;           ^(1 2 1)   ;
                       ;;            ^(2 2 1)  ;
                       ;;             ^(0 3 1) ;
                       ;;              ^(0 4 1);
                       ;;        (1 4 1)^      ;
                       ;;         (2 4 1)^     ;
                       ;;          (#\) 1)^    ;
                       ;;             (0 2)^   ;
                       ;;              (1 2)^  ;
                       ;;               (2 2)^ ;
                       ;;                (#\))^;
                       ))))
   (and)))
|#
