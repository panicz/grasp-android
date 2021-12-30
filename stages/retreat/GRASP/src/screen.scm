(import (define-syntax-rule))
(import (define-interface))
(import (define-type))
(import (for))
(import (examples))


(define-type (Extent width: real
                     height: real))

(define-interface Screen ()
  (paren-width)::real
  (clear!)::void
  (translate! x::real y::real)::void
  (draw-string! s::string left::real top::real)::Extent
  (draw-text! s::string left::real top::real)::real
  (draw-atom! text::string)::Extent
  (open-paren! height::real left::real top::real)::void
  (close-paren! height::real left::real top::real)::void
  )

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

(define (last-line s::string)::string
  (let ((n (string-length s))
        (last-newline 0))
    (for i from 0 below n
         (if (eq? (string-ref s i) #\newline)
             (set! last-newline (+ i 1))))
    (substring s last-newline n)))


(e.g.
 (last-line "\
abc
def") ===> "def")


(define-simple-class NullScreen (Screen)
  ((paren-width)::real 0)
  
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
  
  ((open-paren! height::real left::real top::real)::void
   (values))
  
  ((close-paren! height::real left::real top::real)::void
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
