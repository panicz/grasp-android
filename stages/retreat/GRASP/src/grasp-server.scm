(import
 (cell-display-properties)
 (define-interface)
 (define-type)
 (conversions)
 (primitive)
 (text-screen)
 (combinators)
 (parse)
 (examples)
 (assert)
 (for)
 (match)
 (infix))

(define-alias server java.net.ServerSocket)
(define-alias socket java.net.Socket)
(define-alias input-stream java.io.InputStream)
(define-alias output-stream java.io.OutputStream)

(define (read-delim #!optional
                    (delim #\newline)
                    (port (current-input-port)))
  (let ((delim/int (char->integer delim))
        (first (read-u8 port)))
    (cond
     ((eof-object? first)
      first)
     ((eqv? first delim/int)
      "")
     (else
      (let ((result (cons (integer->char first) '())))
        (define (read-into last-tail)
          (let ((c (read-u8 port)))
            (cond ((or (eof-object? c)
                       (eqv? c delim/int))
                   result)
                  (else
                   (set! (tail last-tail) (cons (integer->char c) '()))
                   (read-into (tail last-tail))))))
        (list->string (read-into result)))))))

(define parsed (head (call-with-input-string "\
(define (factorial n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))" parse)))

(define screen ::Screen (TextScreen))

(set! (current-screen) screen)

(draw! parsed)

(define server ::server (server 5432))

(let* ((connection ::socket (server:accept))
       (input ::input-stream (connection:getInputStream))
       (output ::output-stream (connection:getOutputStream)))
  
  (define (respond response)
    (write-bytevector (string->utf8
                       (if (string? response)
                           response
                           (with-output-to-string
                             (lambda ()
                               (write response)
                               (newline)))))
                      output)
    (force-output output))
  
  (define (process-input)
    (let ((line (read-delim #\newline input)))
      (unless (eof-object? line)
        (let ((message (parse-string line)))
          (respond
           (match message
             (`(cursor-next)
	      ;; kursor nie powinien byc reprezentowany wewnatrz ekranu,
	      ;; tylko na zewnatrz (ale wewnatrz sesji!), bo teoretycznie
	      ;; dopuszczamy mozliwosc, ze jest wielu uzytkownikow
	      ;; (albo wiele widokow) i kazdy ma jakies swoje zestawy
	      ;; kursorow (i ewentualnie selekcji).
	      ;;
	      ;; z drugiej strony, ekran juz w tej chwili jest
	      ;; dosc niezalezny od danej, na ktorej pracuje,
	      ;; przez co wydaje sie, ze kursor moze byc czescia
	      ;; ekranu
	      
	      
              (screen:cursor-next!)
              `(,(screen:cursor-left) ,(screen:cursor-top)))
             
             (`(cursor-back)
              (screen:cursor-back!)
              `(,(screen:cursor-left) ,(screen:cursor-top)))
             
             (`(cursor-up)
              (screen:cursor-up!)
              `(,(screen:cursor-left) ,(screen:cursor-top)))
             
             (`(cursor-down)
              (screen:cursor-down!)
              `(,(screen:cursor-left) ,(screen:cursor-top)))
             
             (`(screen-state)
              (screen:toString))

	     (`(select-left)
	      `(,(screen:cursor-left) ,(screen:cursor-top)))

	     (`(select-right)
	      `(,(screen:cursor-left) ,(screen:cursor-top)))

	     (`(select-up)
	      `(,(screen:cursor-left) ,(screen:cursor-top)))

	     (`(select-down)
	      `(,(screen:cursor-left) ,(screen:cursor-top)))

	     (`(create-box)
	      `(,(screen:cursor-left) ,(screen:cursor-top)))

	     (`(cursor-leave)
	      `(,(screen:cursor-left) ,(screen:cursor-top)))

	     (`(insert-character ,c)
	      `(,(screen:cursor-left) ,(screen:cursor-top)))
	     
             (_
              (display "Unsupported message: ")
              (display message)
              (display " list? ")(display (list? message))
              (display (length message))
              (display (head message))
              (display (eq? (head message) 'cursor-next))
              (newline)
              'unsupported-message))))
        (process-input))))

  (process-input))
