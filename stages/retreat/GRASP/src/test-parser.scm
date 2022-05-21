(import
 (indexable)
 (primitive)
 (space)
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


(define line-comments-with-empty-input
  (with-input-from-string "\
; line comments with empty input
" parse-document))

(show line-comments-with-empty-input)
(newline)

(define nested-expression-comments (parse-string "\
(begin
#; (move 10 #;bytes #;(from (beautiful)) source #;to target)
)
"))

(show nested-expression-comments)
(newline)

(define nested-block-comments (parse-string "\
hey!
#| what follows is an empty list
#| and this comment is nested |#|#
( #|this is the aforementioned empty list|#
#|and it seems to work|# )
"))

(show nested-block-comments)
(newline)

