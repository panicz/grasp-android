(import (match))
(import (examples))
(import (infix))

(define (drop k::integer #;elements-from s::list)::list
  (if (is k <= 0)
      s
      (drop (- k 1) #;elements-from (cdr s))))

(e.g.
 (drop 2 '(1 2 3))
 ===> (3))

(define (drop-after! k::integer #;elements-in s::list)::list
  (define (lastmost-tail n::integer l::list)
    (if (or (<= n 1) (isnt l pair?))
	l
	(lastmost-tail (- n 1) (cdr l))))
  (let ((trail (lastmost-tail k s)))
    (when (pair? trail)
      (set! (cdr trail) '())))
  s)


(e.g.
 ;; if the input is a pair, then the output must also be a pair
 ;; (we cannot return an empty list)
 (let ((items (list 'a 'b 'c)))
   (drop-after! 0 items))
 ===> (a))

(e.g.
 ;; but if the input is not a pair, we get it back no problem
 (drop-after! 100 '()) ===> ())

(e.g.
 (let ((items (list 'a 'b 'c)))
   (drop-after! 1 items))
 ===> (a))

(e.g.
 (let ((items (list 'a 'b 'c)))
   (drop-after! 2 items))
 ===> (a b))

(e.g.
 (let ((items (list 'a 'b 'c)))
   (drop-after! 5 items))
 ===> (a b c))


(define (suffix? ending::list stem::list)::boolean
  (let ((m ::integer (length ending))
        (n ::integer (length stem)))
    (and (is m <= n)
         (let ((r ::integer (- n m)))
           (equal? (drop r stem) ending)))))

(e.g.
 (is '(4 5) suffix? '(1 2 3 4 5)))

(define (fold-left f x0 xs)
  (match xs
    ('() x0)
    (`(,h . ,t) (fold-left f (f x0 h) t))))

(e.g.
 (fold-left (lambda (a b) `(,a + ,b)) 'e '(a b c d))
 ===> ((((e + a) + b) + c) + d))

(define (fold-right f x0 xs)
  (match xs
    ('() x0)
    (`(,h . ,t) (f h (fold-right f x0 t)))))

(e.g.
 (fold-right (lambda (a b) `(,a + ,b)) 'e '(a b c d))
 ===> (a + (b + (c + (d + e)))))

(define (only cool? stuff)
  (match stuff
    ('()
     '())
    (`(,first . ,rest)
     (if (cool? first)
	 `(,first . ,(only cool? rest))
	 (only cool? rest)))))

(e.g.
 (only even? '(1 2 3 4 5 6))
 ===> (2 4 6))

(define (any satisfying? elements)
  (and-let* ((`(,first . ,rest) elements))
    (or (satisfying? first)
	(any satisfying? rest))))

(e.g.
 (any even? '(1 2 3)))

(define (every satisfying? elements)
  (or (null? elements)
      (and-let* ((`(,first . ,rest) elements))
	(and (satisfying? first)
	  (every satisfying? rest)))))

(e.g.
 (every even? '(2 4 6)))

(define (in element list)
  (any (is _ equal? element) list))

(define (union set . sets)
  (define (union a b)
    (fold-left (lambda (set element)
		 (if (is element in set)
		     set
		     `(,element . ,set)))
	       a b))
  (fold-left union set sets))

(e.g.
 (union '(a b c) '(b c d e))
 ===> (e d a b c))

(define (intersection set . sets)
  (define (intersection a b)
    (only (is _ in b) a))
  (fold-left intersection set sets))

(e.g.
 (intersection '(a b c) '(b c d) '(c d e))
 ===> (c))

(define (difference set . sets)
  (define (difference a b)
    (fold-left (lambda (set element)
		 (if (is element in set)
		     (only (isnt _ equal? element) set)
		     set))
	       a b))
  (fold-left difference set sets))

(e.g.
 (difference '(a b c) '(b c d))
 ===> (a))

(define (subset? a b)
  (every (is _ in b) a))

(e.g.
 (is '(a b) subset? '(b a c)))

(define (same-sets? a b)
  (and (is a subset? b)
       (is b subset? a)))

(e.g.
 (same-sets? '(a b c) '(b a c)))

(define (concatenate list)
  (apply append list))

(e.g.
 (concatenate '((a b) (c) (d e f)))
 ===> (a b c d e f))

(define (pass x . functions)
  (fold-left (lambda (x f) (f x)) x functions))

(define (last list::pair)
  (if (pair? (cdr list))
      (last (cdr list))
      (car list)))

(e.g.
 (last '(1 2 3)) ===> 3)

(define (last-pair list::pair)::pair
  (if (pair? (cdr list))
      (last-pair (cdr list))
      list))

(e.g.
 (last-pair '(1 2 3)) ===> (3 . ()))
