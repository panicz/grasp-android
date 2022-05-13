(import
 (indexable)
 (primitive)
 (parse)
 (examples)
  )

(define factorial-definition (parse-string "\
(define (factorial n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))
"))

(show (car factorial-definition))
(newline)

(define string-append-example (parse-string "\
(e.g. 
  (string-append \"abc\" \"def\")
   ===> \"abcdef\")
"))

(show (car string-append-example))
(newline)
