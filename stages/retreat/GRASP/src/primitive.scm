(import (define-syntax-rule))
(import (assert))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (define-property))
(import (define-cache))
(import (match))
(import (examples))
(import (infix))

(import (extent))
(import (indexable))
(import (for))
(import (screen))

;;(import (rename (keyword-arguments) (define/kw define*)))

(define-interface Tile (Indexable)
  (draw! screen::Screen
	 cursor::Cursor
	 context::Cursor)::Extent
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

(define-object (cons a d)::Tile
  (define (equals object)::boolean
   (eq? object (this)))

  (define (hash-code)::int
    (java.lang.System:identity-hash-code (this)))

  (define (draw! screen::Screen cursor::Cursor context::Cursor)::Extent
    (parenthesized! (lambda (object screen cursor context)
                      (draw-sequence! object
                                      screen: screen
                                      cursor: cursor
                                      context: context))
                    (this)
                    screen: screen
                    cursor: cursor
                    context: context))

  (define (has-children?)::boolean #t)
  
  (define (part-at index::Index)::Indexable*
    (if (or (eq? index #\() (eq? index #\)))
	(this)
	(cell-index (this) (as int index))))
  
  (define (first-index)::Index
    #\()
   
  (define (last-index)::Index
    #\))
  
  (define (next-index index::Index)::Index
    (match index
      (#\( 0)
      (#\) #\))
      (,@(is _ < (last-cell-index (this)))
       (+ index 1))
      (_
       #\))))
  
  (define (previous-index index::Index)::Index
    (match index
      (0 #\()
      (#\) (last-cell-index (this)))
      (#\( #\()
      (_ (- index 1))))

  (pair a d))

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

(define-object (Symbol source::string)::Tile
  (define name :: string)
  
  (define (draw! screen::Screen cursor::Cursor context::Cursor)::Extent
    (screen:draw-atom! name))

  (define (has-children?)::boolean #f)
  
  (define (part-at index::Index)::Indexable*
    (this))

  (define (first-index)::Index
    0)
  
  (define (last-index)::Index
    (string-length name))
  
  (define (next-index index::Index)::Index
    (min (last-index) (+ index 1)))
  
  (define (previous-index index::Index)::Index
    (max 0 (- index 1)))

  (gnu.mapping.SimpleSymbol ((source:toString):intern))
  (set! name source))

(define (draw-sequence! elems #!key
                        (screen :: Screen (current-screen))
                        (cursor::Cursor '())
                        (context::Cursor '()))
  ::Extent
  (let (;(cursor-map ::CursorMap (LinearCursorMap))
	(max-width 0)
        (max-line-height (screen:min-line-height))
        (top 0)
        (left 0)
        (index 0))

    (define (skip-spaces! spaces::string)::void
      (for char in spaces
           (cond ((eq? char #\newline)
                  ;; powinnismy dodac wszystkie obiekty
		  ;; z tej linii do detekcji
                  (set! top (+ top max-line-height))
                  (set! left 0)
                  (set! max-line-height
		    (screen:min-line-height)))
                 (else
                  (set! left (+ left 1))
                  (set! max-width (max max-width left)))))
      (set! index (+ index 1)))

    (define (advance! extent::Extent)::void
      (set! left (+ left extent:width))
      (set! max-line-height
	(max extent:height max-line-height))
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
      ;;(as Extent cursor-map)
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
	     ;;(as Extent cursor-map)
             (Extent width: max-width
                     height: (+ top max-line-height)))))
    
    (draw-next! elems)
    ))

;; RZM37UHSPY5Z

;; no dobra, jak powinien dzialac cursor-next?
;; 1. bierzemy element spod kursora i pytamy go o pierwszy
;; pod-indeks
;; 2. jezeli nie ma takowego, to pytamy rodzica o kolejmy 
;; pod-indeks wzgledem naszego
;; 3. i teraz uwaga: jezeli rodzic nie ma kolejnego
;; indeksu, to powinien zapytac dziadka itd. (az dojdziemy
;; do Adama/Ewy)



