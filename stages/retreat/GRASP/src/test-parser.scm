(import
  (parse)
  (indexable)
  (primitive)
  (examples)
  )

(define parsed (parse-string "\
(define (factorial n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))"))

(show parsed)
