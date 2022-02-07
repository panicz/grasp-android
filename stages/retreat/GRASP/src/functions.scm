(import (match))
(import (examples))
(import (infix))

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
