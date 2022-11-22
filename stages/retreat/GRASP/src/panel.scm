(import (srfi :17))
(import (hash-table))
(import (define-syntax-rule))
(import (assert))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (default-value))
(import (define-parameter))
(import (define-cache))
(import (mapping))
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
(import (parameterize-up))

(define-interface Drag ()
  (move! x::real y::real dx::real dy::real)::void
  (drop! x::real y::real vx::real dy::real)::void
  )

(define-mapping (dragging finger::byte)::Drag #!null)

(define-interface Panel ()
  (draw! context::Cursor)::void
  (tap! finger::byte #;at x::real y::real)::boolean
  (press! finger::byte #;at x::real y::real)::boolean

  (key-pressed! key-code)::boolean
  (key-released! key-code)::boolean
  (key-typed! unicode)::boolean
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

(define-enum HorizontalSplitFocus (Left Right))

(define-type (HorizontalSplit at: rational
			      left: Panel
			      right: Panel
			      focus: HorizontalSplitFocus
			      := HorizontalSplitFocus:Left)
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
  ((tap! finger::byte #;at x::real y::real)::boolean
   (let* ((painter (the-painter))
	  (extent (the-panel-extent))
	  (line-width (invoke painter 'vertical-line-width))
          (inner-width (- extent:width
			  line-width))
          (left-width (* at inner-width))
          (right-width (- inner-width left-width)))
     (cond ((is x < left-width)
	    (set! focus HorizontalSplitFocus:Left)
	    (left:tap! finger #;at x y))
	   ((is (+ left-width line-width) < x)
	    (set! focus HorizontalSplitFocus:Right)
	    (right:tap! finger #;at (- x left-width line-width) y
			)))))

  ((press! finger::byte #;at x::real y::real)::boolean
   (let* ((painter (the-painter))
	  (extent (the-panel-extent))
	  (line-width (invoke painter 'vertical-line-width))
          (inner-width (- extent:width
			  line-width))
          (left-width (* at inner-width))
          (right-width (- inner-width left-width)))
     (cond ((is x < left-width)
	    (set! focus HorizontalSplitFocus:Left)
	    (left:press! finger #;at x y))
	   ((is (+ left-width line-width) < x)
	    (set! focus HorizontalSplitFocus:Right)
	    (right:press! finger #;at (- x left-width line-width) y
			  )))))
  
  ((key-pressed! key-code)::boolean
   (match focus
     (,HorizontalSplitFocus:Left
      (left:key-pressed! key-code))
     (,HorizontalSplitFocus:Right
      (right:key-pressed! key-code))))

  ((key-released! key-code)::boolean
   (match focus
     (,HorizontalSplitFocus:Left
      (left:key-released! key-code))
     (,HorizontalSplitFocus:Right
      (right:key-released! key-code))))

  ((key-typed! key-code)::boolean
   (match focus
     (,HorizontalSplitFocus:Left
      (left:key-typed! key-code))
     (,HorizontalSplitFocus:Right
      (right:key-typed! key-code))))
  )

(define-mapping (on-key-press code)::(maps () to: boolean)
  never)

(define-mapping (on-key-release code)::(maps () to: boolean)
  never)

(define-mapping (on-key-type code)::(maps (Any) to: boolean)
  never)

(define-object (Editor)::Panel
  (define document (cons '() '()))
  (define cursor :: Cursor '())

  (define selection-anchor :: Cursor '())
  
  (define (draw! context::Cursor)::void
    (parameterize ((the-document document)
		   (the-cursor cursor)
		   (the-selection-anchor selection-anchor))
      (draw-sequence! (head document))))
  
  (define (tap! finger::byte #;at x::real y::real)::boolean
    (parameterize/update-sources ((the-document document))
      (set! cursor (cursor-under x y))
      (set! selection-anchor cursor)
      (display cursor)
      (display (the-expression at: cursor))
      (newline)
      #t))

  (define (press! finger::byte #;at x::real y::real)::boolean
    #t)
  
  (define (key-pressed! key-code)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-selection-anchor
				   selection-anchor))
      ((on-key-press key-code))))
  
  (define (key-released! key-code)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-selection-anchor
				   selection-anchor))
      ((on-key-release key-code))))
  
  (define (key-typed! unicode)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-selection-anchor
				   selection-anchor))
      ((on-key-type unicode) unicode)))

  )
  
(define-parameter (the-top-panel) ::Panel
  (Editor))
