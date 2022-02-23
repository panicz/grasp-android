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
(import (functions))
(import (print))


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

#|
 0 2 4 6
( a . b )
  1 3 5
|#


;; take-cell-at! returns either a cons-cell whose
;; car is the desired object, or a head/tail-separator
;; (consider: what to do when cursor points to a space?
;; we can safely return #!null, because it's different
;; than returning (#!null))
(define (take-cell-at! cursor::Cursor expression::pair)
  (match cursor
    (`(,,@(isnt _ integer?) . ,root)
     (take-cell-at! root expression))
    
    (`(,,@(is _ <= 1) ,parent-index . ,root)
     (let* ((grandparent ::pair (cursor-ref expression root))
	    (cell (drop (quotient parent-index 2) grandparent))
	    (removed (head cell)))
       (if (dotted? removed)
	   (let ((new (cons (tail removed) '())))
	     (tail-space-to-head removed new)
	     (set! (head cell) new)
	     (unset! (dotted? removed)))
	   (set! (head cell) (tail removed)))
       (set! (tail removed) '())
       removed))
    
    (`(,index . ,root)
     (let* ((parent ::pair (cursor-ref expression root))
	    (index (quotient index 2))
	    (irrelevant (- index 1)))
       (define (remove-tail! preceding)
	 (let ((removed (tail preceding)))
	   (set! (tail preceding) (tail removed))
	   (set! (tail removed) '())
	   removed))
	 
       (if (is irrelevant > 0)
	   (let* ((irrelevant (- irrelevant 1))
		  (preceding (drop irrelevant parent)))
	     (if (dotted? preceding)
		 (let* ((removed (tail-space-to-head
				  preceding
				  (cons (tail preceding) '()))))
		   (set! (tail preceding) '())
		   (unset! (dotted? preceding))
		   removed)
		 (remove-tail! (tail preceding))))
	   (let ((preceding (drop irrelevant parent)))
	     (if (dotted? preceding)
		 (let* ((added (cons (tail preceding) '())))
		   (tail-space-to-head preceding added)
		   (set! (tail preceding) added)
		   (unset! (dotted? preceding))
		   head/tail-separator)
		 (remove-tail! preceding))))))
    (_
     expression)))

(define (take-part-at! cursor::Cursor object)
  (cond #;((Indexable? object)
	 (invoke (as Indexable object) 'take-part-at! cursor))

   ((pair? object)
    (take-cell-at! cursor object))

   (else
    (error "Don't know how to take "cursor" from "object))))

(e.g.
 (let* ((document `(,1 ,3 ,5))
	(taken (take-cell-at! '(3) document)))
   (and (equal? document '(1 5))
	(equal? taken '(3)))))

(e.g.
 (let* ((document `(,1 ,3 ,5))
	(taken (take-cell-at! '(5) document)))
   (and (equal? document '(1 3))
	(equal? taken '(5)))))

(e.g.
 (let* ((document `((,1 ,3 ,5)))
	(taken (take-cell-at! '(1 1) document)))
   (and (equal? document '((3 5)))
	(equal? taken '(1)))))

(e.g.
 (let* ((document `((,1 . ,5)))
	(taken (take-cell-at! '(1 1) document)))
   (and (equal? document '((5)))
	(equal? taken '(1)))))

(e.g.
 (let* ((document `((,1 . ,5)))
	(taken (take-cell-at! '(3 1) document)))
   (and (equal? document '((1 5)))
	(head/tail-separator? taken))))

(e.g.
 (let* ((document `((,1 . ,5)))
	(taken (take-cell-at! '(5 1) document)))
   (and (equal? document '((1)))
	(equal? taken '(5)))))

(define (put-into-cell-at! cursor::Cursor element #;in document)
  (assert (or (and (pair? element)
		   (null? (tail element)))
	      (head/tail-separator? element)))
  (match cursor
    (`(,,@(isnt _ integer?) . ,root)
     (put-into-cell-at! root element document))

    (`(,,@(is _ <= 1) ,parent-index . ,root)
     (assert (pair? element))
     (let* ((grandparent ::pair (cursor-ref document root))
	    (parent (drop (quotient parent-index 2) grandparent)))
       (set! (tail element) (head parent))
       (set! (head parent) element)))

    (`(,index . ,root)
     (let* ((parent (cursor-ref document root))
	    (irrelevant (- (quotient index 2) 1))
	    (preceding (drop irrelevant parent)))
       (cond ((pair? element)
	      (set! (tail element) (tail preceding))
	      (set! (tail preceding) element))
	     
	     ((null? (tail (tail preceding)))
	      (assert (head/tail-separator? element))
	      (set! (tail preceding) (head (tail preceding)))
	      (update! (dotted? preceding) #t)))))
    (_
     (values))
  ))


(e.g.
 (let ((document `((,1 ,5))))
   (put-into-cell-at! '(2 1) `(,3) document)
   document) ===> ((1 3 5)))

(e.g.
 (let ((document `((,1 ,5))))
   (put-into-cell-at! '(2 1)
		      head/tail-separator
		      document)
   document) ===> ((1 . 5)))

(e.g.
 (let ((document `((,3 ,5))))
   (put-into-cell-at! '(0 1) `(,1) document)
   document) ===> ((1 3 5)))


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
      (when (equal? cursor (recons index context))
	;;(WARN "skip-spaces! - cursor: "(recons index context))
	(screen:remember-offset! (- left 1) top))
      (when (even? index)
	(set! index (+ index 1))))

    (define (advance! extent::Extent)::void
      (when (equal? cursor (recons index context))
	;;(WARN "advance! - cursor: "(recons index context))
	(screen:remember-offset! left top))
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
               (with-translation screen (previous-left previous-top)
				 (screen:draw-vertical-bar!
				  max-line-height)))))
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



