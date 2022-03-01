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
	 context::Cursor)::void
	 
  (extent screen::Screen)::Extent
  )

(define (draw! object #!key
               (screen::Screen (current-screen))
               (cursor::Cursor '())
               (context::Cursor '()))
  ::void
  (cond ((instance? object Tile)
	 (invoke (as Tile object) 'draw! screen cursor context))

	(else
	 (error "Don't know how to draw "object))))

(define (extent object #!optional
		(screen::Screen (current-screen)))::Extent
  (cond ((instance? object Tile)
	 (invoke (as Tile object) 'extent screen))

	(else
	 (error "Don't know how to draw "object))))

(define-syntax-rule (with-translation screen (x y) . actions)
  (let ((x! x)
        (y! y))
    (screen:translate! x! y!)
    (begin . actions)
    (screen:translate! (- x!) (- y!))))

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

  (define (draw! screen::Screen cursor::Cursor context::Cursor)::void
    (let ((inner (sequence-extent (this) screen))
	  (paren-width (screen:paren-width)))
      (screen:open-paren! inner:height 0 0)
      (with-translation screen (paren-width 0)
	(draw-sequence! (this)
			screen: screen
			cursor: cursor
			context: context))
      (screen:close-paren! inner:height
			   (+ paren-width inner:width) 0)))

  (define (extent screen::Screen)::Extent
    (let ((extent ::Extent (sequence-extent (this) screen)))
      (Extent width: (+ extent:width (* 2 (screen:paren-width)))
	      height: extent:height)))
    
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

(define-syntax recons*
  (syntax-rules ()
    ((_ a b)
     (recons a b))
    
    ((_ a b c ...)
     (recons a (recons* b c ...)))))

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


;; Docelowo bedziemy musieli patchowac Kawe, chociazby po to,
;; zeby takie obiekty, jak Symbol czy FString (IString? MString?)
;; implementowaly Tile.
;;
;; Na razie jednak zrobmy tak, zeby nie uzywac tych typow
;; bezposrednio w edytorze, a zamiast tego korzystac sobie
;; z owijek

(define-object (Symbol source::string)::Tile
  (define name :: string)
  
  (define (draw! screen::Screen cursor::Cursor context::Cursor)::void
    (screen:draw-atom! name))

  (define (extent screen::Screen)::Extent
    (Extent width: (screen:atom-width name)
	    height: (screen:min-line-height)))
  
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

(define (empty-space-extent spaces::string screen::Screen)::Extent
  (let ((inner (string-extent spaces)))
    (Extent width: (+ inner:width
		      (* 2 (screen:paren-width)))
	    height: inner:height)))

(define (head-extent pair::cons screen::Screen)::Extent
  (if (null? (head pair))
      (empty-space-extent (null-head-space pair) screen)
      (extent (head pair) screen)))


(define (tail-extent pair::cons screen::Screen)::Extent
  (if (null? (tail pair))
      (empty-space-extent (null-tail-space pair) screen)
      (extent (tail pair) screen)))

(define (skip-first-line s::string)::string
  (let ((n (string-index s (is _ eq? #\newline))))
    (if n
	(string-drop s (+ n 1))
	"")))

(e.g.
 (skip-first-line "abc
def") ===> "def")

(define (should-the-bar-be-horizontal? dotted-pair::cons)::boolean
  (assert (dotted? dotted-pair))
  (and (string-any (is _ eq? #\newline)
		   (post-head-space dotted-pair))
       (string-any (is _ eq? #\newline)
		   (pre-tail-space dotted-pair))))

(define (draw-sequence! elems::cons #!key
                        (screen :: Screen (current-screen))
                        (cursor::Cursor '())
                        (context::Cursor '()))
  ::void
  (let ((max-width 0)
        (max-line-height (screen:min-line-height))
        (top 0)
        (left 0)
        (index 0))

    (define (skip-spaces! spaces::string)::void
      (for char in spaces
           (cond ((eq? char #\newline)
                  (set! top (+ top max-line-height))
                  (set! left 0)
                  (set! max-line-height
		    (screen:min-line-height)))
                 (else
                  (set! left (+ left 1))
                  (set! max-width (max max-width left)))))
      (when (equal? cursor (recons index context))
	(screen:remember-offset! (- left 1) top))
      (set! index (+ index 1)))

    (define (advance! extent::Extent)::void
      (when (equal? cursor (recons index context))
	(screen:remember-offset! left top))
      (set! left (+ left extent:width))
      (set! max-line-height (max extent:height max-line-height))
      (set! max-width (max left max-width))
      (set! index (+ index 1)))

    (define (draw-empty-list! spaces::string)::void
      (let ((inner (empty-space-extent spaces screen))
	    (paren-width (screen:paren-width)))
	(screen:open-paren! inner:height 0 0)
	(screen:close-paren! inner:height
			     (+ paren-width inner:width) 0)))
    
    (define (draw-head! pair::cons)::void
      (let ((context (recons index context)))
        (with-translation screen (left top)
          (if (null? (head pair))
              (draw-empty-list! (null-head-space pair))
              (draw! (head pair)
                     screen: screen
                     cursor: (subcursor cursor context)
                     context: context)))))

    (define (draw-dotted-tail! pair::cons)::void
      (cond ((should-the-bar-be-horizontal? pair)
	     (with-translation screen (0 top)
	       (screen:draw-horizontal-bar! max-width))
             (skip-spaces! (skip-first-line (pre-tail-space pair)))
             (let ((context (recons index context)))
	       (with-translation screen (left top)
		 (if (null? (tail pair))
		     (draw-empty-list! (null-tail-space
					pair))
		     (draw! (tail pair)
			    screen: screen
			    cursor: (subcursor cursor
					       context)
			    context: context))))
             (advance! (tail-extent pair screen))
             (skip-spaces! (post-tail-space pair)))
            (else
	     (with-translation screen (left top)
	       (screen:draw-vertical-bar!
		max-line-height))
             (advance! (Extent width: (screen:vertical-bar-width)
			       height: 0))
             (skip-spaces! (pre-tail-space pair))
             (let ((context (recons index context)))
	       (with-translation screen (left top)
		 (if (null? (tail pair))
		     (draw-empty-list! (null-tail-space pair))
		     (draw! (tail pair)
			    screen: screen
			    cursor: (subcursor cursor
					       context)
			    context: context))))
             (advance! (tail-extent pair screen))
             (skip-spaces! (post-tail-space pair))
             )))

    (define (draw-next! pair::cons)::void
      (draw-head! pair)
      (advance! (head-extent pair screen))
      (skip-spaces! (post-head-space pair))
      (cond ((dotted? pair)
             (draw-dotted-tail! pair))

            ((pair? (tail pair))
             (draw-next! (tail pair)))))

    (skip-spaces! (pre-head-space elems))
    (draw-next! elems)
    ))

(define (cursor-under left::real top::real
		      elems
		      #!key
		      (screen::Screen (current-screen))
		      (context::Cursor '()))
  ::Cursor
  (let ((box (extent elems screen))
	(max-width 0)
	(max-line-height (screen:min-line-height))
	(side 0)
	(ceiling 0)
	(index (first-index elems)))

    (define (check-spaces! spaces::string
			   #!optional (i ::int 0))
      ::Cursor
      (cond ((is i >= (string-length spaces))
	     #!null)
	    
	    ((eq? (spaces i) #\newline)
	     (set! ceiling (+ ceiling max-line-height))
	     (set! side 0)
	     (set! max-line-height
		   (screen:min-line-height))
	     (cond ((is top < ceiling)
	            (recons* i index context))
		   (else
		    (set! index (next-index index elems))
		    (check-spaces! spaces (+ i 1)))))
	    
	    (else
	     (set! side (+ side 1))
	     (set! max-width (max max-width side))
	     (cond ((and (is side < left <= (+ side 1))
			 (is top <= max-line-height))
		    (recons* i index context))
		   (else
		    (set! index (next-index index elems))
		    (check-spaces! spaces (+ i 1)))))))
      
    (define (advance! extent::Extent)::Null
      (set! side (+ side extent:width))
      (set! max-line-height (max extent:height max-line-height))
      (set! max-width (max side max-width))
      (set! index (next-index index elems))
      #!null)

    (define (check! part extent::Extent)::Cursor
      (if (and (is side <= left <= (+ side extent:width))
	       (is ceiling <= top <= (+ ceiling extent:height)))
	  (let ((cursor (recons index context)))
	    (or (and (pair? part)
		     (cursor-under (- left side)
				   (- top ceiling)
				   part
				   screen: screen
				   context: cursor))
		cursor))
	  (advance! extent)))

    (define (check-separating-bar! pair)::Cursor
      (cond ((should-the-bar-be-horizontal? pair)
	     (let ((bar-height (screen:horizontal-bar-height)))
	       (if (is ceiling <= top <= (+ ceiling bar-height))
		   (recons index context)
		   (advance! (Extent width: 0
				     height: bar-height)))))
	    (else
	     (let ((bar-width (screen:vertical-bar-width)))
	       (if (is side <= left (+ side bar-width))
		   (recons index context)
		   (advance! (Extent width: bar-width
				     height: 0)))))))

    (define (check-next! pair)::Cursor
      (or (check! (head pair) (head-extent pair screen))
	  (check-spaces! (post-head-space pair))
	  (and (dotted? pair)
	       (or (check-separating-bar! pair)
		   (check-spaces! (pre-tail-space pair))
		   (check! (tail pair) (tail-extent pair screen))
		   (check-spaces! (post-tail-space pair))))
	  (and (pair? pair)
	       (check-next! pair))))

    (or (and (is 0 <= left < (screen:paren-width))
	     (is 0 <= top < box:height)
	     (recons index context))
	(check-spaces! (pre-head-space elems))
	(check-next! elems)
	(and (is (- box:width
		    (screen:paren-width)) <= left <= box:width)
	     (is 0 <= top < box:height)
	     (recons index context))
	#!null)))

(define (sequence-extent elems screen::Screen)::Extent
  (let ((max-width 0)
        (max-line-height (screen:min-line-height))
        (top 0)
        (left 0))

    (define (skip-spaces! spaces::string)::void
      (for char in spaces
        (cond ((eq? char #\newline)
               (set! top (+ top max-line-height))
               (set! left 0)
               (set! max-line-height
		     (screen:min-line-height)))
              (else
               (set! left (+ left 1))
               (set! max-width (max max-width left))))))

    (define (advance! extent::Extent)::void
      (set! left (+ left extent:width))
      (set! max-line-height (max extent:height max-line-height))
      (set! max-width (max left max-width)))

    (define (dotted-tail-extent pair::cons)::Extent
      (cond ((should-the-bar-be-horizontal? pair)
	     (skip-spaces! (skip-first-line (pre-tail-space pair)))
	     (advance! (tail-extent pair screen))
	     (skip-spaces! (post-tail-space pair)))
	    (else
	     (advance! (Extent width: (screen:vertical-bar-width)
			       height: 0))
	     (skip-spaces! (pre-tail-space pair))
	     (advance! (tail-extent pair screen))
	     (skip-spaces! (post-tail-space pair))))
      (Extent width: max-width
              height: (+ top max-line-height)))

    (define (grow-ahead! pair)
      (advance! (head-extent pair screen))
      (skip-spaces! (post-head-space pair))
      (cond ((dotted? pair)
             (dotted-tail-extent pair))

            ((pair? (tail pair))
             (grow-ahead! (tail pair)))

            (else
             (Extent width: max-width
                     height: (+ top max-line-height)))))

    (skip-spaces! (pre-head-space pair))
    (grow-ahead! elems)
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



