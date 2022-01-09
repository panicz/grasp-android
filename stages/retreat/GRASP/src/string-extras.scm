(import (examples))
(import (match))
(import (extent))
(import (for))
(import (infix))

(define (string-extent s::string)::Extent
  (let ((line-length 0)
        (max-length 0)
        (total-lines 1))
    (for c in s
         (cond ((eq? c #\newline)
                (set! max-length (max max-length line-length))
                (set! total-lines (+ total-lines 1))
                (set! line-length 0))
               (else
                (set! line-length (+ line-length 1)))))
    (Extent width: (max max-length line-length)
            height: total-lines)))

(e.g.
 (string-extent "\
abc
def") ===> [Extent width: 3 height: 2])

(define (string-last-line s::string)::string
  (let ((n (string-length s))
        (last-newline 0))
    (for i from 0 below n
         (if (eq? (string-ref s i) #\newline)
             (set! last-newline (+ i 1))))
    (substring s last-newline n)))

(e.g.
 (string-last-line "\
abc
def") ===> "def")

(define (suffix? x y)
  (or (equal? x y)
      (and-let* ((`(,h . ,t) y))
        (suffix? x t))))

(e.g.
 (is '(c d e) suffix? #;of '(a b c d e)))
