
(define *random-number-generator* ::java.util.Random (java.util.Random))

(define (random #!optional (bound::real 1.0))
  (if (and (exact? bound)
	   (integer? bound))
      (invoke *random-number-generator* 'nextInt (as int bound))
      (* bound (invoke *random-number-generator* 'nextFloat))))

(define (bad-example form expected-value obtained-value)
  (call-with-output-string
   (lambda (port)
     (display "while evaluating\n" port)
     (display form port)
     (display "\n\nExpected:\n" port)
     (display expected-value port)
     (display "\nGot: \n" port)
     (display obtained-value port))))

(define-syntax e.g.
  (syntax-rules (===> $bracket-list$ $string$)
    ((_ example)
     (or example
	 (error 'example)))
    ((_ example ===> value)
     (let ((result example))
       (if (equal? result 'value)
	   result
	   (error (bad-example 'example 'value result)))))
    ))

(define predicate procedure)

(define (every satisfying?::predicate elements::list)::boolean
  (or (null? elements)
      (and (satisfying? (car elements))
	   (every satisfying? (cdr elements)))))

(define (any satisfying?::predicate elements::list)
  (and (not (null? elements))
       (or (satisfying? (car elements))
	   (any satisfying? (cdr elements)))))

(define (fold-left f x0 . xs*)
  (define (fold-left1 f x0 xs)
    (if (null? xs)
	x0
	(fold-left1 f (f x0 (car xs)) (cdr xs))))

  (define (fold-left2 f x0 xs xs2)
    (if (or (null? xs) (null? xs2))
	x0
	(fold-left2 f (f x0 (car xs) (car xs2)) (cdr xs) (cdr xs2))))

  (define (fold-left3 f x0 xs xs2 xs3)
    (if (or (null? xs) (null? xs2) (null? xs3))
	x0
	(fold-left3 f (f x0 (car xs) (car xs2) (car xs3)) (cdr xs) (cdr xs2) (cdr xs3))))

  (define (fold-left* f x0 . xs*)
    (if (any null? xs*)
	x0
	(apply fold-left* f (apply f x0 (map car xs*)) (map cdr xs*))))
  (let ((n (length xs*)))
    (case n
      ((0) x0)
      ((1) (fold-left1 f x0 (car xs*)))
      ((2) (fold-left2 f x0 (car xs*) (cadr xs*)))
      ((3) (fold-left3 f x0 (car xs*) (cadr xs*) (caddr xs*)))
      (else (apply fold-left* f x0 xs*)))))

(e.g.
 (fold-left (lambda (x y)
	      `(,x + ,y))
	    'e
	    '(a b c d))
 ===> ((((e + a) + b) + c) + d))

(define-syntax matrix
  (syntax-rules (rows: columns: ::)
    ((matrix rows: n columns: m)
     list)
    ((matrix rows: n::t1 columns: m::t2)
     list)
    ((matrix rows: n columns: m::t2)
     list)
    ((matrix rows: n::t1 columns: m)
     list)
    ))

(define (transpose M::(matrix rows: ,m::int columns: ,n::int))
  ::(matrix rows: n columns: m)
  (apply map list M))

(define (M*2 A::(matrix rows: ,k columns: ,m)
	     B::(matrix rows: ,m columns: ,n))
  ::(matrix rows: ,k columns: ,n)
  (let ((B^T (transpose B)))
    (map (lambda (rA)
	   (map (lambda (cB)
		  (apply + (map * rA cB)))
		B^T))
	 A)))

(define (M* M . MM)
  (fold-left M*2 M MM))

(e.g.
 (M* '((1 2 3)
       (4 5 6)) '((7)
		  (8)
		  (9)) '((50 122))) ===> ((2500  6100)
					  (6100 14884)))

(define (M+2 A::(matrix rows: ,m columns: ,n)
	     B::(matrix rows: ,m columns: ,n))
  ::(matrix rows: ,m columns: ,n)
  (map (lambda (rA rB)
	 (map + rA rB))
       A B))

(define (M+ M . MM)
  (fold-left M+2 M MM))

(e.g.
 (M+ '((1 2)
       (3 4)) '((5 6)
		(7 8))) ===> (( 6  8)
			      (10 12)))

;; to, co teraz bedziemy robic, wydaje sie troche
;; skomplikowane.
;; ale oto, co robimy:

(define (read-all #!optional (port (current-input-port)))
  (let ((input (read port)))
    (if (eof-object? input)
	'()
	(let ((result (cons input '())))
	  (let loop ((growth-cone result))
	    (let ((input (read port)))
	      (cond ((eof-object? input)
		     result)
		    (else
		     (set! (cdr growth-cone) (cons input '()))
		     (loop (cdr growth-cone))))))))))

(define (last-pair list::pair)::pair
  (if (pair? (cdr list))
      (last-pair (cdr list))
      list))

(e.g.
 (last-pair '(1 2 3)) ===> (3 . ()))

(define (concatenate! list-of-lists)
  (if (null? list-of-lists)
      '()
      (if (null? (car list-of-lists))
	  (concatenate! (cdr list-of-lists))
	  (let* ((result (car list-of-lists)))
	    (let loop ((last-segment result)
		       (rest (cdr list-of-lists)))
	      (cond ((null? rest)
		     result)
		    ((null? (car rest))
		     (loop last-segment (cdr rest)))
		    (else
		     (set! (cdr (last-pair last-segment)) (car rest))
		     (loop (car rest) (cdr rest)))))))))

(define (append! . lists)
  (concatenate! lists))

(define (fold-right f x0 . xs*)
  (define (fold-right1 f x0 xs)
    (if (null? xs)
	x0
	(f (car xs) (fold-right1 f x0 (cdr xs)))))

  (define (fold-right2 f x0 xs xs2)
    (if (or (null? xs) (null? xs2))
	x0
	(f (car xs) (car xs2) (fold-right2 f x0 (cdr xs) (cdr xs2)))))

  (define (fold-right3 f x0 xs xs2 xs3)
    (if (or (null? xs) (null? xs2) (null? xs3))
	x0
	(f (car xs) (car xs2) (car xs3)
	   (fold-right3 f x0 (cdr xs) (cdr xs2) (cdr xs3)))))

  (define (fold-right* f x0 . xs*)
    (if (any null? xs*)
	x0
	(apply f (fold-right1 (lambda (x y)
				(cons (car x) y))
			      (list (apply fold-right* f x0 (map cdr xs*)))
			      xs*))))
  (let ((n (length xs*)))
    (case n
      ((0) x0)
      ((1) (fold-right1 f x0 (car xs*)))
      ((2) (fold-right2 f x0 (car xs*) (cadr xs*)))
      ((3) (fold-right3 f x0 (car xs*) (cadr xs*) (caddr xs*)))
      (else (apply fold-right* f x0 xs*)))))

(e.g. (fold-right (lambda (x y z)
		    `(,x * ,y + ,z))
		  'e
		  '(a b c d)
		  '(P Q R S))
      ===> (a * P + (b * Q + (c * R + (d * S + e)))))

(define (numbers #!key
		 (from::real 0)
		 (to::real 0)
		 (by::real (if (> from to) -1 1)))  
  (if (or (and (> from to) (>= by 0))
	  (and (< from to) (<= by 0)))
      '()
      (let ((result (cons from '())))
	(let loop ((tip result)
		   (from (+ from by)))
	  (if (or (and (> from to) (>= by 0))
		  (and (< from to) (<= by 0)))
	      result
	      (begin
		(set! (cdr tip) (cons from (cdr tip)))
		(loop (cdr tip) (+ from by))))))))

(define (read-samples path n)
  (let* ((filename (number->string n))
	 (path (string-append path filename))
	 (output (map (lambda (m)
			(if (= m n) 1.0 0.0))
		      (numbers from: 0 to: 20)))
	 (data (call-with-input-file path read-all)))
    (map (lambda (input)
	   (list input output))
	 data)))

(define (split! list #!key (at::int 1))
  (let loop ((input list)
	     (pivot at))
    (if (<= pivot 0)
	'()
	(if (= pivot 1)
	    (let ((suffix (cdr input)))
	      (set! (cdr input) '())
	      suffix)
	    (loop (cdr input) (- pivot 1))))))

(e.g.
 (let* ((l (list 1 2 3 4 5))
	(s (split! l at: 3)))
   (and (equal? l '(1 2 3))
	(equal? s '(4 5)))))

(define (take k::integer #;elements-from s::list)::list
  (if (and (pair? s)
	   (> k 0))
      (let ((result (cons (car s) '())))
	(let loop ((input (cdr s))
		   (tip result)
		   (k (- k 1)))
	  (if (or (<= k 0) (null? input))
	      result
	      (begin
		(set! (cdr tip) (cons (car input) (cdr tip)))
		(loop (cdr input) (cdr tip) (- k 1))))))
      s))

(define (drop k::integer #;elements-from s::list)::list
  (if (and (pair? s)
	   (> k 0))
      (let loop ((result (cdr s))
		 (k (- k 1)))
	(if (or (<= k 0) (null? result))
	    result
	    (loop (cdr result) (- k 1))))
      s))

(e.g.
 (drop 3 #;elements-from '(1 2 3 4 5))
 ===> (4 5))

(define (swap-prefixes! a::list b::list)
  (when (and (pair? a) (pair? b))
    (let ((e (car a)))
      (set! (car a) (car b))
      (set! (car b) e)
      (swap-prefixes! (cdr a) (cdr b)))))

(e.g.
 (let ((L1 (list 'a 'b 'c 'd 'e))
       (L2 (list 1 2 3)))
   (swap-prefixes! L1 L2)
   (and (equal? L1 '(1 2 3 d e))
	(equal? L2 '(a b c)))))

(define (splice! sublist::list #!key (into::list '()) (at::int 0))::list
  (let ((suffix (split! into at: at)))
    (append! into sublist suffix)))

(e.g.
 (splice! '(a b c) into: '(1 2 3 4) at: 2) ===> (1 2 a b c 3 4))

(define (shuffle! input::list)::list
  (let ((n (length input)))
    (cond
     ((= n 0) '())
     ((= n 1) input)
     (else
      (let ((suffix (split! input at: (random n))))
	(shuffle! input)
	(shuffle! suffix)
	(cond ((= (random 2) 1)
	       (swap-prefixes! input suffix)
	       (let ((prefix-length (length input))
		     (suffix-length (length suffix)))
		 (if (< suffix-length prefix-length)
		     (splice! suffix into: input at: suffix-length)
		     (append! input suffix))))
	      (else 
	       (append! input suffix))))))))

(define (find-cell satisfying?::predicate elements::list)
  (and (not (null? elements))
       (if (satisfying? elements)
	   elements
	   (find-cell satisfying? (cdr elements)))))

(e.g.
 (find-cell (lambda (cell)
	      (= (car cell) 3))
	    '(1 2 3 4 5))
 ===> (3 4 5))

(define (only cool? stuff)
  (if (null? stuff)
      '()
      (if (cool? (car stuff))
	  (cons (car stuff) (only cool? (cdr stuff)))
	  (only cool? (cdr stuff)))))

(define-syntax while
  (syntax-rules ()
    ((while condition actions ...)
     (let ()
       (define (loop)
	 (when condition
	   actions ... (loop)))
       (loop)))))

(define-syntax assert
  (syntax-rules ()
    ((_ proposition)
     (or proposition (error "Assertion failed: " 'proposition)))))

(define (mangle . lists)
  (let  ((result '()))
    (while (not (null? lists))
      (let loop ((input lists)
		 (previous #f))
	(when (pair? input)
	  (if (null? (car input))
	      (if (pair? previous)
		  (set! (cdr previous) (cdr input))
		  (begin
		    (assert (eq? lists input))
		    (set! lists (cdr input))))
	      (begin
		(set! result (cons (caar input) result))
		(set! (car input) (cdr (car input)))))
	  (loop (cdr input) input))))
    result))

(e.g.
 (mangle '(1 2 3) '(a b c d) '(P Q))
 ===>  (d c 3 Q b 2 P a 1))

(define (activation layer)
  (car layer))

(define (weights layer)
  (cadr layer))

(define (weighted-sum list-1 list-2 #!key (from 0))
  (fold-left (lambda (z x y)
	       (+ z (* x y)))
	     from
	     list-1
	     list-2))

(define (propagate input #;through network)
  (fold-left
   (lambda (feed layer)
     (let* ((output (map (lambda (neuron)
			   (weighted-sum (cdr neuron) feed
					 from: (car neuron)))
			 (weights layer))))
       (map (activation layer) output)))
   input
   network))

(define (derivative function)
  (procedure-property function 'derivative))

(set! (setter derivative)
      (lambda (function value)
	(set! (procedure-property function 'derivative) value)))

(define (inverse function)
  (procedure-property function 'inverse))

(set! (setter inverse)
      (lambda (function value)
	(set! (procedure-property function 'inverse) value)))

(define (square x) (* x x))

(set! (derivative square) (lambda (x) (* 2 x)))

(set! (inverse square) sqrt)

(define (identity x) x)

(set! (derivative identity) (lambda (x) 1))

(set! (inverse identity) identity)

(define (sigmoid x) (/ (+ 1 (exp (- x)))))

(set! (derivative sigmoid)
      (lambda (x) (/ (exp (- x)) (square (+ 1 (exp (- x)))))))

(set! (inverse sigmoid)
      (lambda (x) (- (log (- (/ 1 x) 1)))))

(define (random-weights inputs::int outputs::int)
  ::(matrix rows: outputs columns: inputs)
  (map (lambda (row)
	 (map (lambda (column)
		(- (random 2.0) 1.0))
	      (numbers from: 0 to: inputs)))
       (numbers from: 1 to: outputs)))

(define (random-network . throughputs)
  (let ((n (length throughputs)))
    (map (lambda (index inputs outputs)
	   `(,(if (= index (- n 1))
		  identity
		  sigmoid)
	     ,(random-weights inputs outputs)))
	 (numbers from: 1 to: n)
	 throughputs
	 (cdr throughputs))))

(e.g.
 (let ((layers `((,sigmoid
		  ((0.80109 -0.46122 0.97314 -0.39203)
		   (0.43529 0.78548 2.10584 -0.57847)))
		 (,identity
		  ((-0.23680 -0.81546  1.03775)))))
       (inputs '((23 75 176)
		 (25 67 180)
		 (28 120 175))))
   (map (lambda (input)
	  (propagate input layers))
	inputs))
 ===> ((0.798534188006313)
       (0.8009499165011527)
       (-0.0145099999999998)))

(define (half-mean-square-error network data-set)
  ;; We consider "half-mean-square-error", because
  ;; its derivative simplifies to the difference
  ;; between obtained and expected output
  (/ (apply + (map (lambda (training-sample)
		     (let ((input (car training-sample))
			   (output (cadr training-sample)))
		       (let ((result (propagate input network)))
			 (apply + (map (lambda (obtained expected)
					 (square (- obtained expected)))
				       result output)))))
		   data-set))
     (length data-set)
     2))

(define (trace excitation #;through network)
  (fold-left
   (lambda (propagation layer)
     (let* ((inputs (car propagation))
	    (neurons (weights layer))
	    (stimulation (map (lambda (neuron)
				(weighted-sum (cdr neuron) inputs
					      from: (car neuron)))
			      neurons))
	    (output (map (activation layer) stimulation)))
       `(,output . ,propagation)))
   `(,excitation)
   network))

(define the-network
  (random-network 512 128 64 20))

(half-mean-square-error the-network (take 20 training-samples))

;; No dobra. To algorytm dziala tak,
;; ze wyliczamy sobie bledy na poszczegolnych
;; warstwach, zaczynajac od konca.

(define (map! f inout . in*)
  (let ((n (length in*)))
    (case n
      ((0)
       (let loop ((tip inout))
	 (if (pair? tip)
	     (begin
	       (set! (car tip) (f (car tip)))
	       (loop (cdr tip)))
	     inout)))
      ((1)
       (let loop ((tip1 inout)
		  (tip2 (car in*)))
	 (if (and (pair? tip1) (pair? tip2))
	     (begin
	       (set! (car tip1) (f (car tip1) (car tip2)))
	       (loop (cdr tip1) (cdr tip2)))
	     inout)))
      ((2)
       (let loop ((tip1 inout)
		  (tip2 (car in*))
		  (tip3 (cadr in*)))
	 (if (and (pair? tip1) (pair? tip2) (pair? tip3))
	     (begin
	       (set! (car tip1) (f (car tip1) (car tip2) (car tip3)))
	       (loop (cdr tip1) (cdr tip2) (cdr tip3))
	       inout))))
      (else
       (let loop ((tip inout)
		  (tips in*))
	 (if (and (pair? tip) (every pair? tips))
	     (begin
	       (set! (car tip) (apply f (car tip) (map car tips)))
	       (loop (cdr tip) (map! cdr tips)))
	     inout))))))

(e.g.
 (let ((M (list (list 1 2 3)
		(list 4 5 6))))
   (matrix-add! M '((12 10 8)
		    ( 6  4 2)) with-factor: 1/2)
   M) ===> ((7 7 7)
	    (7 7 7)))

(define (weight-improvements network input desired-output)
  "The backpropagation algorithm: given a neural network,
an input datum and a desired output, produce - for each layer
of the network - matrices of gradients"
  (define (influence layer)
    (let ((firing (activation layer)))
      (lambda (excitation)
	((derivative firing) ((inverse firing) excitation)))))
  
  (let* ((excitations (trace input #;through network))
	 (reverse-network (reverse network))
	 (delta (map (let ((influence (influence (car reverse-network))))
		       (lambda (output target)
			 (* (- output target) (influence output))))
		     (car excitations) desired-output))
	 (deltas (fold-left
		  (lambda (deltas layer excitation)
		    (let* ((influence (influence layer))
			   (delta (map (lambda (neuron result)
					 (* (influence result)
					    (weighted-sum neuron (car deltas))))
				       (cdr (transpose (weights layer)))
				       excitation)))
		      `(,delta . ,deltas)))
		  `(,delta)
		  reverse-network
		  (cdr excitations))))
    (map (lambda (delta excitation)
	   (map cons delta (M* (transpose (list delta))
			       (list excitation))))
	 (cdr deltas) (reverse excitations))))

(e.g.
 (weight-improvements `((,sigmoid ((.35 .15 .20)
				   (.35 .25 .30)))
			(,sigmoid ((.60 .40 .45)
				   (.60 .50 .55))))
		      '(.05 .1)
		      '(.01 .99))
 ===> (((0.008771354689486937 4.3856773447434685E-4 8.771354689486937E-4)
	(0.009954254705217202 4.977127352608601E-4 9.954254705217202E-4))
       ((0.13849856162855698 0.08216704056423078 0.08266762784753326)
	(-0.03809823651655623 -0.022602540477475067 -0.02274024221597822))))

(define (matrix-add! target-matrix increment-matrix #!key (with-factor 1))
  (map! (lambda (target-row increment-row)
	  (map! (lambda (target-value increment-value)
		  (+ target-value (* with-factor increment-value)))
		target-row
		increment-row))
	target-matrix
	increment-matrix))

(define (improve! network #!key by (with 1))
  (map! (lambda (layer gradients)
	  (matrix-add! layer gradients with-factor: with))
	network
	by))

