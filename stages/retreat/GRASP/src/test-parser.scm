(import
 (indexable)
 (primitive)
 (parse)
 (examples)
 (conversions)
 (srfi :11)
 (assert)
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

(define something-with-line-comments (parse-string "\
; this input contains line comments
( ; in various places 
 + ; like, after an operator
 2 ; and an operand
 3 ; and at the end of a list
)

( ;as well as in an empty list
)

;and at the end of input
"))

(show something-with-line-comments)
(newline)


;; sadly, it doesn't work for empty input

(define line-comments-with-empty-input (parse-string "\
; line comments with empty input
"))

(show line-comments-with-empty-input)
(newline)

(display (with-input-from-string ";;aaa dupa" read-list))
