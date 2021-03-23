(use-modules
 (grand scheme)
 (grand define-keywords)
 (rnrs base))

(define (read-sequence/eof? sequence)
  (if (null? sequence)
      sequence
      (let ((c (read-char))
	    (`(,char . ,sequence*) sequence))
	(if (eof-object? c)
	    '()
	    (or (and-let* (((eq? c char))
			   (result (read-sequence/eof?
				    sequence*)))
		  (if (eq? result sequence*)
		      sequence
		      `(,c . ,result)))
		(begin
		  (unread-char c)
		  #false))))))

(e.g.
 (with-input-from-string "abc d"
   (lambda ()
     (let ((s (read-sequence/eof? '(#\a #\b #\d))))
       (values s (read)))))
 ===> #f abc)

(e.g.
 (with-input-from-string "abc d"
   (lambda ()
     (let ((s (read-sequence/eof? '(#\a #\b #\c))))
       (values s (read)))))
 ===> (#\a #\b #\c) d)

(e.g.
 (with-input-from-string "ab"
   (lambda ()
     (read-sequence/eof? '(#\a #\b #\c))))
 ===> (#\a #\b))

"on success, read-sequence/eof? returns its argument
(so the value returned is eq? to the argument)"

(e.g.
 (let ((prefix '(#\a #\b #\c)))
   (eq? (with-input-from-string "abc"
	  (lambda ()
	    (read-sequence/eof? prefix)))
	prefix)))


(e.g.
 (let* ((prefix '(#\a #\b #\c))
	(result (with-input-from-string "ab"
		  (lambda ()
		    (read-sequence/eof? prefix))))
	(prefix- (take prefix 2)))
   (and (equal? prefix- result)
	(not (eq? prefix- result)))))

;; kilka uwag:
;; jezeli idzie o atomy (tzn. symbole, liczby, stringi, itd)
;; to nie wczytujemy ich jako znaki, tylko jako liczby&typy.
;; dopiero po przeczytaniu całego inputu
;; 1. zamieniamy go na string
;; 2. wszystkie atomy konwertujemy biorąc substring
;; i odpowiedni typ
;; 


(define-syntax-rule (define-type type-name . details)
  "
At the moment, the sole purpose of `type definitions'
is to document the `records' used later in the code.

Over time, a more elaborate type system can be developed.
")


(define-syntax-rule (declare . type)
  "
This form is used for declaring types of functions
and other objects. Like with `define-type', they have
no effect, but it might change in the future.
")


(define-type Source
  #:whitespace Natural
  #:start Natural
  #:end Natural)

(define-type Atom
  (either
   Symbol
   Number
   Boolean
   Char
   String))

(define-type SExpression
  (extend Atom
   Pair
   List))

(define-type HollowSyntaxTree
  (either
   (extend
    Source
    (either
     (List #:reverse-elements (HollowSyntaxTree ...)
	   #:last-tail HollowSyntaxTree
	   #:ultimate-whitespace-start Natural
	   #:ultimate-whitespace-end Natural)
     (Atom #:value Atom)))
   ;; the "Silent" node is used to represent the last tail
   ;; of a proper list (another strategy would be to
   ;; introduce different type constructors for proper
   ;; and improper lists, but it would be more burdensome)
   (Silent #:value Atom)))

(define (delimiter? c)
  (or (eof-object? c)
      (char-whitespace? c)
      (memq c '(#\( #\) #\" #\;))))

(define comment-delimiters
  '(((#\;) (#\newline))
    ((#\# #\|) (#\| #\#))))

(declare skip-comment (current-input-port) :
	 ((Char ...) (Char ...) Natural
	  -> (Char ...) Natural))


(define* (skip-comment suffix chars #:= '() position #:= 0)
  (cond
   ((eof-object? (peek-char))
    (values chars position))
   
   ((read-sequence/eof? suffix)
    => (lambda (suffix)
	 (values
	  `(,@(reverse suffix) ,@chars)
	  (+ position (length suffix)))))
   (else
    (skip-comment suffix
		  `(,(read-char) . ,chars)
		  (+ position 1)))))

(declare skip-whitespace (current-input-port) :
	 ((Char ...) Natural -> (Char ...) Natural))

(define* (skip-whitespace chars #:= '() position #:= 0)
  (cond
   ((eof-object? (peek-char))
    (values chars position))
   
   ((any (is (first _) read-sequence/eof?)
	 comment-delimiters)
    => (lambda (prefix)
	 (let* ((`(,suffix) (or (assoc-ref comment-delimiters
					   prefix)
				'(())))
		(n (length prefix))
		(chars position
		       (skip-comment suffix
				     `(,@(reverse prefix)
				       ,@chars)
				     (+ position n))))
	   (skip-whitespace chars position))))
   
   ((read-sequence/eof? '(#\# #\;))
   => (lambda (prefix)
	(let ((_ chars post-expr-position
		     (read-expression
		      #:collecting `(,@(reverse prefix)
				     . ,chars)
		      #:at (+ position (length prefix)))))
	  (skip-whitespace chars post-expr-position))))
   
   ((char-whitespace? (peek-char))
    (skip-whitespace `(,(read-char) . ,chars)
		     (+ position 1)))
   (else
    (values chars position))))

(declare skip-until (current-input-port) :
	 ((Char -> Any) (Char ...) Natural
	  -> (Char ...) Natural))

(define* (skip-until condition? chars #:= '() position #:= 0)
  (if (condition? (peek-char))
      (values chars position)
      (skip-until condition?
		  `(,(read-char) . ,chars)
		  (+ position 1))))

(declare read-expressions (current-input-port) :
	 (#:collecting (char ...)
	  #:at natural
	  #:into (HollowSyntaxTree ...)
	  #:from natural
	  #:following natural
	  -> (HollowSyntaxTree : List) (char ...) natural))

(define* (read-expressions
	  #:collecting chars #:= '()
	  #:at position #:= 0
	  #:into reverse-elements #:= '()
	  #:from opening-position #:= position
	  #:following whitespace-position #:= position)
  (let* ((chars position/token
		(skip-whitespace chars position))
	 (c (read-char)))
    (cond
     ((or (eq? c #\))
	  (eof-object? c))
      (let ((chars final-position
		   (if (eof-object? c)
		       (values chars position/token)
		       (values `(,c . ,chars)
			       (+ position/token 1)))))
	(values
	 `(List
	   #:whitespace ,whitespace-position
	   #:start ,opening-position
	   #:end ,final-position
	   #:reverse-elements ,reverse-elements
	   #:last-tail (Silent #:value ())
	   #:ultimate-whitespace-start ,position
	   #:ultimate-whitespace-end ,position/token)
	 chars
	 final-position)))
     
     ((and (eq? c #\.)
	   (delimiter? (peek-char)))
      (let* ((expr chars after-expression
		   (read-expression #:collecting `(,c . ,chars)
				    #:at (+ position/token 1)
				    #:following position))
	     (chars ultimate-whitespace
		    (skip-whitespace chars after-expression))
	     (c (read-char))
	     (chars final-position
		    (if (eof-object? c)
			(values chars ultimate-whitespace)
			(values `(,c . ,chars)
				(+ ultimate-whitespace 1)))))
	(assert (or (eq? c #\)) (eof-object? c)))
	(values
	 `(List
	   #:whitespace ,whitespace-position
	   #:start ,opening-position
	   #:end ,final-position
	   #:reverse-elements ,reverse-elements
	   #:last-tail ,expr
	   #:ultimate-whitespace-start ,after-expression
	   #:ultimate-whitespace-end ,ultimate-whitespace)
	 chars
	 final-position)))
     
     (else
      (unread-char c)
      (let ((expr chars position
		  (read-expression #:collecting chars
				   #:at position/token
				   #:following position)))
	(read-expressions #:collecting chars
			  #:at position
			  #:into `(,expr . ,reverse-elements)
			  #:from opening-position
			  #:following whitespace-position)))
     )))

(define* (read-string
	  #:collecting chars #:= '()
	  #:at position #:= 0
	  #:from starting-position #:= position
	  #:following whitespace-position #:= position)
  (let ((c (read-char)))
    (cond
     ((eof-object? c)
      (values
       `(Atom #:whitespace ,whitespace-position
	      #:start ,starting-position
	      #:end ,position)
       chars
       position))
     ((and (eq? c #\")
	   (even? (length (span (is _ eq? #\\) chars))))
      (values
       `(Atom #:whitespace ,whitespace-position
	      #:start ,starting-position
	      #:end ,(+ position 1))
       `(,c . ,chars)
       (+ position 1)))
     (else
      (read-string
       #:collecting `(,c . ,chars)
       #:at (+ position 1)
       #:from starting-position
       #:following whitespace-position)))))

(declare read-expression (current-input-port) :
	 (#:collecting (Char ...)
	  #:at Natural
	  #:from Natural
	  #:following Natural
	  -> HollowSyntaxTree (Char ...) Natural))

(define* (read-expression
	  #:collecting chars #:= '()
	  #:at position #:= 0
	  #:following whitespace-position #:= position)
  (let* ((chars position/expression
		(skip-whitespace chars position)))
    (cond
     ((eq? (peek-char) #\()
      (read-expressions #:collecting
			`(,(read-char) . ,chars)
			#:at (+ position/expression 1)
			#:from position/expression
			#:following whitespace-position))
     ((eq? (peek-char) #\")
      (read-string #:collecting `(,(read-char) . ,chars)
		   #:at (+ position/expression 1)
		   #:from position/expression
		   #:following whitespace-position))
     
     (else
      (let ((chars position/final
		   (skip-until delimiter? chars
			       position/expression)))
	(values
	 `(Atom #:whitespace ,whitespace-position
		#:start ,position/expression
		#:end ,position/final)
	 chars
	 position/final))))))

(define-type Whitespaced
  #:whitespace String)

(define-type ConcreteSyntaxTree
  (extend
   Whitespaced
   (either
    (List #:reverse-elements (ConcreteSyntaxTree ...)
	  #:last-tail ConcreteSyntaxTree
	  #:ultimate-whitespace String)
    (Atom #:source-string
	  #:value Atom))))

(declare substantiate :
	 (String -> (HollowSyntaxTree -> ConcreteSyntaxTree)))

(define ((substantiate string) tree)
  (match tree
    (`(Atom #:whitespace ,whitespace
	    #:start ,start
	    #:end ,end)
     (let ((whitespace (substring/read-only
			string whitespace start))
	   (source-string (substring/read-only
			   string start end)))
       `(Atom #:whitespace ,whitespace
	      #:source-string ,source-string
	      #:value ,(with-input-from-string source-string
			read))))
    (`(List #:whitespace ,whitespace
	    #:start ,start
	    #:end ,end
	    #:reverse-elements ,reverse-elements
	    #:last-tail ,last-tail
	    #:ultimate-whitespace-start
	    ,ultimate-whitespace-start
	    #:ultimate-whitespace-end
	    ,ultimate-whitespace-end)
     (let ((whitespace (substring/read-only
			string whitespace start))
	   (source-string (substring/read-only
			   string start end))
	   (ultimate-whitespace (substring/read-only
				 string
				 ultimate-whitespace-start
				 ultimate-whitespace-end)))
       `(List #:whitespace ,whitespace
	      ;;#:source-string ,source-string
	      #:reverse-elements ,(map (substantiate string)
				       reverse-elements)
	      #:last-tail ,((substantiate string) last-tail)
	      #:ultimate-whitespace
	      ,ultimate-whitespace)))
    (`(Silent #:value ,value)
     `(Atom #:whitespace ""
	    #:source-string ""
	    #:value ,value))
    (_
     (error "Unsupported expression" tree))))

(declare parse (current-input-port) :
	 (-> ConcreteSyntaxTree string))

(define (parse)
  (let ((tree chars total (read-expression)))
    (assert (= (length chars) total))
    (let ((string (list->string (reverse chars))))
      (values
       ((substantiate string) tree)
       string))))

(declare parse-all (current-input-port) :
	 (-> (ConcreteSyntaxTree : List) string))

(define (parse-all)
  (let ((tree chars total (read-expressions)))
    (assert (= (length chars) total))
    (let ((string (list->string (reverse chars))))
      (values
       ((substantiate string) tree)
       string))))

(declare raw-expression : (ConcreteSyntaxTree -> SExpression))

(define (raw-expression parse-tree)
  (match parse-tree
    (`(Atom #:whitespace ,whitespace
	    #:source-string ,source-string
	    #:value ,value)
     value)
    (`(List #:whitespace ,whitespace
	    ;;#:source-string ,source-string
	    #:reverse-elements ,reverse-elements
	    #:last-tail ,last-tail
	    #:ultimate-whitespace ,ultimate-whitespace)
     (fold-left (lambda (tail head)
		  `(,(raw-expression head) . ,tail))
		(raw-expression last-tail)
		reverse-elements))))

(declare reconstruct-string : (ConcreteSyntaxTree -> string))

(define (reconstruct-string #;from parse-tree)
  (match parse-tree
    (`(Atom #:whitespace ,whitespace
	    #:source-string ,source-string
	    #:value ,value)
     (string-append/shared whitespace source-string))
    (`(List #:whitespace ,whitespace
	    ;;#:source-string ,source-string
	    #:reverse-elements ,reverse-elements
	    #:last-tail ,last-tail
	    #:ultimate-whitespace ,ultimate-whitespace)
     (string-append/shared
      whitespace
      "("
      (fold-left (lambda (tail head)
		   (string-append/shared
		    (reconstruct-string head)
		    tail))
		 (reconstruct-string last-tail)
		 reverse-elements)
      ultimate-whitespace
      ")"))))

(e.g.
 (let* ((input "\
;; (initial, external) whitespace 
( #|ultimate-whitespace|# ) 
;; ignored (unread) whitespace\
")
	(tree (with-input-from-string input parse))
	(s-expr (raw-expression tree))
	(reconstructed (reconstruct-string tree)))
   (assert (is reconstructed string-prefix? input))
   (values s-expr reconstructed))
 ===> () "\
;; (initial, external) whitespace 
( #|ultimate-whitespace|# )")


(e.g.
 (let* ((input "\
;; (initial, external) whitespace
(#|internal whitespace|# x #;ultimate-whitespace ) 
;; external whitespace not followed by any expression
")
	(tree (with-input-from-string input parse-all))
	(s-expr (raw-expression tree))
	(reconstructed (reconstruct-string tree)))
   (and (equal? s-expr '((x)))
	(string=? (string-append "(" input ")")
		  reconstructed))))

(e.g.
 (raw-expression
  (with-input-from-string "   (  \"a\\\"  b\"   )" parse))
 ===> ("a\"  b"))

(e.g.
 (raw-expression
  (with-input-from-string " ( a #;white . #;space b )" parse))
 ===> (a . b))

(e.g. (raw-expression
       (with-input-from-string "
(define (! #;int n) ; -> int
  (if (= n 0)
     #|base case|# 1 ; must be 1, because 0 would break
     (* n (! (- n 1 #|one|#)))))" parse))
      ===> (define (! n)
	     (if (= n 0)
		 1
      		 (* n (! (- n 1))))))
