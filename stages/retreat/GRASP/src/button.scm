(import (define-interface))
(import (define-type))
(import (define-object))
(import (extent))
(import (match))
(import (infix))
(import (functions))
(import (indexable))
(import (painter))
(import (cursor))
(import (tile))
(import (primitive))
(import (extension))
(import (print))


(define-type (Button action: procedure
		     label: string)
  extending Magic
  with
  ((draw! context::Cursor)::void
   (let* ((painter ::Painter (the-painter))
	  (inner ::Extent (string-extent label)))
     (invoke painter 'draw-rounded-rectangle!
	     (+ inner:width 4)
	     (+ inner:height 2))
     (with-translation (2 1)
	 (invoke painter 'draw-string! label
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
  (define (create-from source::cons)::Enchanted
    (try-catch
     (or (as Button (eval source)) #!null)
     (ex java.lang.Throwable
	 (WARN "Unable to create Button from "source": "
	       (java.lang.String:valueOf ex))
	 #!null))))

(set! (extension 'Button) (ButtonExtension))
