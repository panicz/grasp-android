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
(import (document-operations))
(import (space))

(define-alias List java.util.List)
(define-alias ArrayList java.util.ArrayList)

(define-interface Drag ()
  (move! x::real y::real dx::real dy::real)::void
  (drop! x::real y::real vx::real vy::real)::void
  )

(define-interface Drawable ()
  (draw!)::void
  )

(define-object (Overlay)::Drawable
  (define elements :: List[Drawable] (ArrayList[Drawable]))
  
  (define (draw!)::void
    (for element::Drawable in elements
      (element:draw!)))
  
  (define (add! element::Drawable)::void
    (elements:add element))
  
  (define (remove! element::Drawable)::void
    (elements:remove element))
  )

(define overlay ::Overlay (Overlay))

(define-object (Selected sequence::cons)::Drawable
  (define items ::cons sequence)
  (define (draw!)::void
    (parameterize ((the-document items))
      (let ((position ::Position (screen-position items)))
	(with-translation (position:left position:top)
	    (draw-sequence! items)))))
  )

(define-object (DragAround target::Selected)::Drag
  (define selected ::Selected target)
  
  (define (move! x::real y::real dx::real dy::real)::void
    (let ((position ::Position (screen-position selected:items)))
      (set! position:left (+ position:left dx))
      (set! position:top (+ position:top dy))))

  (define (drop! x::real y::real vx::real vy::real)::void
    (values))

  (overlay:add! selected))


(define-mapping (dragging finger::byte)::Drag #!null)

(define-interface Panel ()
  (draw! context::Cursor)::void
  (tap! finger::byte #;at x::real y::real)::boolean
  (press! finger::byte #;at x::real y::real)::boolean

  (release! finger::byte #;at x::real y::real
	    #;with vx::real vy::real)
  ::boolean
  
  (move! finger::byte #;to x::real y::real
	 #;by vx::real vy::real)
  ::boolean
  
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

  ((release! finger::byte #;at x::real y::real
	     #;with vx::real vy::real)
   ::boolean
   (let* ((painter (the-painter))
	  (extent (the-panel-extent))
	  (line-width (invoke painter 'vertical-line-width))
          (inner-width (- extent:width
			  line-width))
          (left-width (* at inner-width))
          (right-width (- inner-width left-width)))
     (cond ((is x < left-width)
	    (set! focus HorizontalSplitFocus:Left)
	    (left:release! finger #;at x y #;with vx vy))
	   ((is (+ left-width line-width) < x)
	    (set! focus HorizontalSplitFocus:Right)
	    (right:release! finger
			    #;at (- x left-width line-width) y
				 #;with vx vy)))))

  ((move! finger::byte #;to x::real y::real
	  #;by dx::real dy::real)
   ::boolean
   (let* ((painter (the-painter))
	  (extent (the-panel-extent))
	  (line-width (invoke painter 'vertical-line-width))
          (inner-width (- extent:width
			  line-width))
          (left-width (* at inner-width))
          (right-width (- inner-width left-width)))
     (cond ((is x < left-width)
	    (set! focus HorizontalSplitFocus:Left)
	    (left:move! finger #;to x y #;by dx dy))
	   ((is (+ left-width line-width) < x)
	    (set! focus HorizontalSplitFocus:Right)
	    (right:move! finger
			 #;to (- x left-width line-width) y
			      #;by dx dy)))))
  
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
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-selection-anchor
				   selection-anchor))
      (let-values (((selection-start selection-end) (the-selection)))
	(and-let* ((path (cursor-under x y))
		   (`(,tip . ,subpath) path)
		   (parent ::Element (the-expression at: subpath))
		   (target ::Element (parent:part-at tip))
		   (position ::Position (screen-position target)))
	  (cond
	   ((isnt parent eq? target)
	    (WARN "reached non-final item"))
	   
	   ((isnt dragging clean?)
	    (WARN "should start scrolling or zooming"))
	   
	   ((is target Space?)
	    (WARN "should start drawing a gesture"))
	   
	   ((is selection-start cursor< path cursor< selection-end)
	    (WARN "should move selection"))
	   
	   ((or (is target Atom?)
		(and (is target cons?)
		     (eqv? tip (target:first-index))))
	    ;; powinnismy powiekszyc spacje poprzedzajaca
	    ;; wydobywany element o szerokosc tego elementu
	    ;; podzielona przez (painter:space-width)
	    (let* ((target (take-cell-at! subpath))
		   (selection (Selected target)))
	      (set! (dragging 0) (DragAround selection))
	      ))

	   ((and (is target cons?)
		 (eqv? tip (target:last-index)))
	    (WARN "should start resizing the object"))

	   (else
	    (WARN "really don't know what to do")))
	  #t))))

  (define (release! finger::byte #;at x::real y::real
		    #;with vx::real vy::real)
    ::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-selection-anchor
				   selection-anchor))
      (and-let* ((drag ::Drag (dragging 0)))
	(drag:drop! x y vx vy)
	#t)))

  (define (move! finger::byte #;to x::real y::real
		 #;by dx::real dy::real)
    ::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-selection-anchor
				   selection-anchor))
      (and-let* ((drag ::Drag (dragging 0)))
	(drag:move! x y dx dy)
	#t)))

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
