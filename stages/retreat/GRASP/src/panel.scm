(import (define-syntax-rule))
(import (assert))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (default-value))
(import (define-parameter))
(import (define-cache))
(import (infix))
(import (match))
(import (functions))
(import (for))
(import (while))
(import (fundamental))
(import (indexable))
(import (cursor))
(import (interactive))
(import (primitive))
(import (extent))
(import (parse))
(import (conversions))
(import (painter))
(import (print))

(define-interface Panel ()
  (draw! context::Cursor)::void
  )

;; this parameter must be set by the
;; graphical framework (Lanterna, AWT, ...)
;; and changed every time the hosting
;; window is resized

(define-parameter (the-screen-extent)::Extent
  (Extent width: 0
	  height: 0))

;; At the top level, (the-panel-extent)
;; must be bound to the same object
;; as (the-screen-extent).
;;
(define-parameter (the-panel-extent)::Extent
  (the-screen-extent))

(define-parameter (the-focus)::Cursor '())

(define-type (HorizontalSplit at: rational
			      left: Panel
			      right: Panel)
  implementing Panel
  with
  ((draw! context::Cursor)::void
   (let* ((painter (the-painter))
	  (extent (the-panel-extent))
	  (line-width (invoke painter 'vertical-line-width))
          (inner-width (- extent:width
			  line-width))
          (left-width (* at inner-width))
          (right-width (- inner-width left-width)))
     (with-clip (left-width extent:height)
       (parameterize ((the-panel-extent
		       (Extent
			width: left-width
			height: extent:height)))
	 (invoke left 'draw!
		 (recons 'left context))))
     (with-translation (left-width 0)
       (invoke painter 'draw-vertical-line! 0)
       (with-translation (line-width 0)
	 (with-clip (right-width extent:height)
	   (parameterize ((the-panel-extent
			   (Extent
			    width: right-width
			    height: extent:height)))
	     (invoke right 'draw!
		     (recons 'right context))))))))
  )

(define-object (Editor)::Panel
  (define document (with-input-from-string "\
(define (! n)
\"Computes the product 1*...*n.
It represents the number of per-
mutations of an n-element set.\"
  (if (<= n 0)
      1
      (* n (! (- n 1))))) 
(e.g. (factorial 5) ===> 120)
(Button action: (lambda () (WARN \"button pressed!\"))
        label: \"Press me!\")
" parse-document))
  (define cursor :: Cursor '())
  
  (define (draw! context::Cursor)::void
    (parameterize ((the-document document)
		   (the-cursor cursor))
      (draw-sequence! (head document))))
  )
  
(define-parameter (the-top-panel) ::Panel
  (Editor))
