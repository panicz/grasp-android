(import (define-interface))
(import (define-type))
(import (define-object))
(import (extent))
(import (match))
(import (infix))
(import (functions))
(import (indexable))
(import (screen))
(import (cursor))
(import (tile))
(import (primitive))
(import (extension))
(import (print))


(define-type (Button action: procedure
		     label: string)
  extending Passive
  with
  ((draw! context::Cursor)::void
   (let* ((screen ::Screen (the-screen))
	  (inner ::Extent (string-extent label)))
     (invoke screen 'draw-rounded-rectangle!
	     (+ inner:width 4)
	     (+ inner:height 2))
     (with-translation (2 1)
	 (invoke screen 'draw-string! label
		 (and (pair? (the-cursor))
		      (equal? (cursor-tail) context)
		      (cursor-head))))))
  
  ((extent)::Extent
   (let ((inner ::Extent (string-extent label)))
     (Extent width: (+ inner:width 4)
	     height: (+ inner:height 2))))

  ((key-pressed key::char)::boolean
   (cond ((eq? key #\newline)
	  (action)
	  #t)
	 (else
	  #f)))
   
  ((tapped x::real y::real)::boolean
   (action)
   #t))

(define-object (ButtonExtension)::Extension
  (define (create-from source::cons)::Interactive
    (try-catch
     (or (as Button (eval source)) #!null)
     (ex java.lang.Throwable
	 (WARN "Unable to create Button from "source": "
	       (java.lang.String:valueOf ex))
	 #!null))))

(set! (extension 'Button) (ButtonExtension))
