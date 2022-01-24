(import (match))
(import (examples))

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

(define (pass x . functions)
  (fold-left (lambda (x f) (f x)) x functions))
