(import (define-syntax-rule))
(import (assert))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (define-property))
(import (define-cache))
(import (define-parameter))
(import (default-value))
(import (fundamental))
(import (infix))
(import (match))
(import (functions))
(import (for))
(import (while))
(import (traversal))
(import (panel))
(import (indexable))
(import (painter))
(import (print))
(import (extent))
(import (space))
(import (parse))
(import (conversions))
(import (cursor))
(import (editor-operations))
;;(import (input))
;;(import (primitive))

(define-alias Bundle android.os.Bundle)
(define-alias KeyEvent android.view.KeyEvent)
(define-alias MotionEvent android.view.MotionEvent)
(define-alias Canvas android.graphics.Canvas)
(define-alias AndroidActivity android.app.Activity)
(define-alias AndroidView android.view.View)
(define-alias Paint android.graphics.Paint)
(define-alias Typeface android.graphics.Typeface)
(define-alias InputMethodManager
  android.view.inputmethod.InputMethodManager)
(define-alias Path2D android.graphics.Path)
(define-alias GestureDetector
  android.view.GestureDetector)
(define-alias DisplayMetrics
  android.util.DisplayMetrics)

(define-alias AndroidResources
  android.content.res.Resources)

(define-alias SensorListener
  android.hardware.SensorListener)

(define-syntax-rule (Path (command args ...) ...)
  (let ((path ::Path2D (Path2D)))
    (invoke path 'command (as float args) ...)
    ...
    path))

(define (path-extent path::Path2D)::Extent
  (let ((rect ::android.graphics.RectF (android.graphics.RectF)))
    (path:computeBounds rect #t)
    (Extent width: (rect:width)
	    height: (rect:height))))

(define-type (Font face: Typeface
		   size: real))

(define (load-font name::string activity::android.app.Activity)::Typeface
  (Typeface:createFromAsset (activity:getAssets) name))

(define-syntax define-initializer
  (syntax-rules  (::)
    ((define-initializer (initializer-name object-name::object-type)
       (definition name etc ... initialization)
       ...)
     (begin 
       (definition name etc ... #!null)
       ...
       (define (initializer-name object-name::object-type)::void
	 (set! name initialization)
	 ...)))))

(define-initializer (initialize-activity activity::android.app.Activity)
  (define Basic-Regular ::Typeface
    (load-font "Basic-Regular.otf" activity))

  (define LobsterTwo-Regular ::Typeface
    (load-font "LobsterTwo-Regular.otf" activity))

  (define Oswald-Regular ::Typeface
    (load-font "Oswald-Regular.ttf" activity))

  (define GloriaHallelujah ::Typeface
    (load-font "GloriaHallelujah.ttf" activity))

  (define NotoSerif-Regular ::Typeface
    (load-font "NotoSerif-Regular.ttf" activity))

  (define the-atom-font ::parameter[Font]
    (make-parameter
     (Font face: LobsterTwo-Regular #;Basic-Regular
	   size: 56)))

  (define the-string-font ::parameter[Font]
    (make-parameter
     (Font face: Basic-Regular #;LobsterTwo-Regular
	   size: 40)))

  (define the-comment-font ::parameter[Font]
    (make-parameter
     (Font face: GloriaHallelujah
	   size: 28)))
  
  (define the-log-font ::parameter[Font]
    (make-parameter
     (Font face: Oswald-Regular
	   size: 28)))

  (define the-cursor-offset ::parameter[Position]
    (make-parameter (Position left: 0 top: 32)))

  (define the-cursor-extent ::parameter[Extent]
    (make-parameter (Extent width: 2 height: 32)))

  (define top-left-paren ::Path2D
    (Path
     (moveTo 20 0)
     (quadTo 5 0 0 50)
     (lineTo 10 50)
     (quadTo 10 30 20 30)
     (close)))

  (define top-left-extent ::Extent
    (path-extent top-left-paren))

  (define bottom-left-paren ::Path2D
    (Path
     (moveTo 20 50)
     (quadTo 5 50 0 0)
     (lineTo 10 0)
     (quadTo 10 20 20 20)
     (close)))

  (define bottom-left-extent ::Extent
    (path-extent bottom-left-paren))

  (define top-right-paren ::Path2D
    (Path
     (moveTo 0 0)
     (quadTo 15 0 20 50)
     (lineTo 10 50)
     (quadTo 10 30 0 30)
     (close)))

  (define top-right-extent ::Extent
    (path-extent top-right-paren))

  (define bottom-right-paren ::Path2D
    (Path
     (moveTo 0 50)
     (quadTo 15 50 20 0)
     (lineTo 10 0)
     (quadTo 10 20 0 20)
     (close)))

  (define bottom-right-extent ::Extent
    (path-extent bottom-right-paren))

  (define ctrl-pressed? ::parameter[boolean]
    (make-parameter #f))
  
  (define shift-pressed? ::parameter[boolean]
    (make-parameter #f))
  
  (define alt-pressed? ::parameter[boolean]
    (make-parameter #f))
  
  (define meta-pressed? ::parameter[boolean]
    (make-parameter #f))
  )

(define (INFO . messages)
  (let ((result ::java.lang.StringBuilder
		(java.lang.StringBuilder)))
    (for message in messages
	 (result:append message))
  (android.util.Log:i "grasp-android" (result:toString))))

(define-early-constant paint ::Paint (Paint))

(define-early-constant transparent ::long #x00000000)

(define-early-constant ruler ::Paint (Paint))

(define (text-width text::CharSequence font::Font)::real
  (ruler:setTypeface font:face)
  (ruler:setTextSize font:size)
  (ruler:measureText text))

(define-object (ScreenLogger size)::MessageHandler
  
  (define (display-messages output::Object)::void
    (let* ((canvas ::Canvas (as Canvas output))
	   (font ::Font (the-log-font))
	   (screen-extent ::Extent (the-screen-extent))
	   (top ::float  screen-extent:height))
      (paint:setTypeface font:face)
      (paint:setTextSize font:size)
      (for message in messages
	   (canvas:drawText message 0 top paint)
	   (set! top (- top font:size)))))

  (logger size))
  
(define-object (View source::AndroidActivity)::Painter
  
  (define canvas ::Canvas)

  (define activity ::AndroidActivity)
  
  (define (showKeyboard)::void
    (when (requestFocus)
      (let ((imm ::InputMethodManager
		 (as InputMethodManager
		     (activity:getSystemService
		      android.content.Context:INPUT_METHOD_SERVICE))))
	(imm:showSoftInput (this) InputMethodManager:SHOW_IMPLICIT))))

  (define clipLeft ::real 0)
  (define clipTop ::real 0)
  (define clipWidth ::real +inf.0)
  (define clipHeight ::real +inf.0)

  (define (clip! left::real  top::real
		 width::real height::real)
    ::void
    (canvas:clipRect (as float left) (as float top)
		     (as float (+ left width))
		     (as float (+ top height)))
    (set! clipLeft left)
    (set! clipTop top)
    (set! clipWidth width)
    (set! clipHeight height))
  
  (define (current-clip-width)::real
    clipWidth)
  
  (define (current-clip-height)::real
    clipHeight)
  
  (define (current-clip-left)::real
    clipLeft)
  
  (define (current-clip-top)::real
    clipTop)

  (define shiftLeft ::real 0)
  (define shiftTop ::real 0)

  (define (translate! x::real y::real)::void
    (canvas:translate x y)
    (set! shiftLeft (+ shiftLeft x))
    (set! shiftTop (+ shiftTop y)))
  
  (define (current-translation-left)::real
    shiftLeft)
  
  (define (current-translation-top)::real
    shiftTop)

  (define (horizontal-line-height)::real
    50)
  
  (define (vertical-line-width)::real
    50)

  (define text-color ::long #xff555555)

  (define background-color ::long transparent)
  
  (define (draw-horizontal-line! top::real)::void
    (let* ((left ::float (max 0 (current-clip-left)))
	   (bottom ::float (+ top (horizontal-line-height)))
	   (right ::float (+ left (current-clip-width))))
      (paint:setColor text-color)
      (canvas:drawRect left (as float top) right bottom paint)))
  
  (define (draw-vertical-line! left::real)::void
    (let* ((top ::float (max 0 (current-clip-top)))
	   (right ::float (+ left (vertical-line-width)))
	   (bottom ::float (+ top (current-clip-height))))
      (paint:setColor text-color)
      (canvas:drawRect (as float left) top right bottom paint)))

  (define marked-cursor-position ::Position
    (Position left: 0
	      top: 0))
  
  (define (mark-cursor! +left::real +top::real)::void
    (let* ((cursor-extent (the-cursor-extent))
	   (cursor-offset (the-cursor-offset))
	   (left (+ +left cursor-offset:left))
	   (top (+ +top cursor-offset:top)))
      (set! marked-cursor-position:left (+ (current-translation-left)
					   +left))
      (set! marked-cursor-position:top (+ (current-translation-top)
					  +top))
      (paint:setColor text-color)
      (canvas:drawRect left top
		       (+ left cursor-extent:width)
		       (+ top cursor-extent:height)
		       paint)))
  
  (define (cursor-position)::Position
    marked-cursor-position)

  (define (cursor-height)::real
    (let ((offset ::Position (the-cursor-offset))
	  (extent ::Extent (the-cursor-extent)))
      (+ offset:top extent:height)))
  
  (define selection-drawing-mode? ::boolean #f)
  
  (define (enter-selection-drawing-mode!)::void
    (set! selection-drawing-mode? #t)
    (set! text-color #xffffffff)
    (set! background-color #xff555555))

  (define (exit-selection-drawing-mode!)::void
    (set! selection-drawing-mode? #f)
    (set! text-color #xff555555)
    (set! background-color transparent))

  (define (in-selection-drawing-mode?)::boolean
    selection-drawing-mode?)

  (define (clear!)::void
    (canvas:drawRGB 255 255 255))

  (define (vertical-bar-width)::real
    5)
  
  (define (horizontal-bar-height)::real
    5)
  
  (define (draw-horizontal-bar! width::real)::void
    (paint:setColor text-color)
    (canvas:drawRect 0 0 width (horizontal-bar-height) paint))
    
  (define (draw-vertical-bar! height::real)::void
    (paint:setColor text-color)
    (canvas:drawRect 0 0 (vertical-bar-width) height paint))

  (define (space-width)::real 16)

  (define (draw-rounded-rectangle! width::real height::real)::void
    (paint:setColor text-color)
    (canvas:drawRoundRect 0 0 (as int width) (as int height) 10 10 paint))

  (define (paren-width)::real
    top-left-extent:width)

  (define (min-line-height)::real
    (let ((font ::Font (the-atom-font)))
      (max font:size
	   (+ top-left-extent:height bottom-left-extent:height)
	   (+ top-right-extent:height bottom-right-extent:height))))

  (define (open-paren! height::real)::void
    (let ((line-height (max 0 (- height
				 top-left-extent:height
				 bottom-left-extent:height))))
      (paint:setColor text-color)
      (canvas:drawPath top-left-paren paint)
      (canvas:drawRect 0 top-left-extent:height
		       10 (+ top-left-extent:height line-height)
		       paint)
      (with-translation (0 (+ top-left-extent:height
			      line-height))
	  (canvas:drawPath bottom-left-paren paint))))

  (define (close-paren! height::real)::void
    (let ((line-height (max 0 (- height
				 top-right-extent:height
				 bottom-right-extent:height))))
      (paint:setColor text-color)
      (canvas:drawPath top-right-paren paint)
      (canvas:drawRect (- top-right-extent:width 10)
		       top-right-extent:height
		       top-right-extent:width
		       (+ top-right-extent:height line-height)
		       paint)

      (with-translation (0 (+ top-right-extent:height
			      line-height))
	  (canvas:drawPath bottom-right-paren paint))))

  (define (draw-box! width::real height::real
		     context::Cursor)
    ::void
    (open-paren! height)
    (with-translation ((- width (paren-width)) 0)
	(close-paren! height)))

  (define (draw-text! text::CharSequence
		      font::Font
		      context::Cursor)
    ::void
    (let-values (((selection-start selection-end) (the-selection)))
      (let* ((focused? (and (pair? (the-cursor))
			    (equal? context (cdr (the-cursor)))))
	     (enters-selection-drawing-mode?
	      (and (pair? selection-start)
		   (equal? (cdr selection-start) context)))
	     (exits-selection-drawing-mode?
	      (and (pair? selection-end)
		   (equal? (cdr selection-end) context)))
	     (segment-start 0)
	     (left ::float 0)
	     (lines 1)
	     (height ::float font:size)
	     (string-end (text:length)))
	(parameterize ((the-cursor-extent (Extent width: 2
						  height: height)))
	  (define (render-fragment! segment-end::int)
	    (let* ((fragment (text:subSequence segment-start
					       segment-end))
		   (width (text-width fragment font)))
	      (paint:setColor background-color)
	      (canvas:drawRect left (* (- lines 1) height)
			       (+ left width) (* lines height)
			       paint)
	      (paint:setColor text-color)
	      (canvas:drawText fragment left (* lines height) paint)
	      (set! left (+ left width))))
	  
	  (paint:setTypeface font:face)
	  (paint:setTextSize font:size)
	  (for i from 0 below string-end
	       (when (and focused? (eqv? (car (the-cursor)) i))
		 (render-fragment! i)
		 (set! segment-start i)
		 (mark-cursor! left (* (- lines 1) height)))
	       
	       (when (and enters-selection-drawing-mode?
			  (eqv? (car selection-start) i))
		 (render-fragment! i)
		 (set! segment-start i)
		 (enter-selection-drawing-mode!))
	       
	       (when (and exits-selection-drawing-mode?
			  (eqv? (car selection-end) i))
		 (render-fragment! i)
		 (set! segment-start i)
		 (exit-selection-drawing-mode!))
	       
	       (when (eq? (text:charAt i) #\newline)
		 (render-fragment! i)
		 (set! left 0)
		 (set! lines (+ lines 1))
		 (set! segment-start (+ i 1))))
	  (render-fragment! string-end)
	  (when (and focused? (eqv? (car (the-cursor)) string-end))
	    (mark-cursor! left (* (- lines 1) height)))))))

  (define (draw-string! text::CharSequence context::Cursor)::void
    (draw-text! text (the-string-font) context))

  (define quoted-text-cursor-offset::Position
    (Position left: -1 top: 2))
  
  (define (draw-quoted-text! text::CharSequence context::Cursor)::void
    (parameterize ((the-cursor-offset quoted-text-cursor-offset))
      (draw-string! text context)))

  (define (text-extent text::CharSequence font::Font)::Extent
    (let* ((line-start 0)
	   (lines ::int 1)
	   (line-height ::float font:size)
	   (max-width 0)
	   (string-end (text:length)))
      (for i from 0 below string-end
	   (when (eq? (text:charAt i) #\newline)
	     (set! max-width
		   (max max-width
			(text-width (text:subSequence
				     line-start i)
				    font)))
	     (set! lines (+ lines 1))
	     (set! line-start (+ i 1))))
      (set! max-width
	(max max-width
		 (text-width
		  (text:subSequence line-start string-end)
		  font)))
      (Extent width: max-width
	      height: (* lines line-height))))

  (define (atom-extent text::CharSequence)::Extent
    (let ((inner (text-extent text (the-atom-font))))
      (Extent width: (+ inner:width 8)
	      height: (+ inner:height 16))))

  (define atom-cursor-offset::Position (Position left: 0 top: 4))

  (define atom-frame-color ::long #xffdddddd)

  (define (draw-atom! text::CharSequence context::Cursor)::void
    (let* ((extent (atom-extent text))
	   (font (the-atom-font)))
      (paint:setColor atom-frame-color)
      (canvas:drawRoundRect (as int 0) (as int 28)
			    (as int extent:width)
			    (as int extent:height)
			    12 12 paint)
      (with-translation (4 16)
	  (parameterize ((the-cursor-offset atom-cursor-offset))
	    (draw-text! text font context)))))
  
  (define (text-character-index-under x::real y::real
				      text::CharSequence
				      font::Font)
    ::int
    (let* ((line-height font:size)
	   (string-end (text:length)))
      (let loop ((i 0)
		 (left 0)
		 (top 0))
	(if (is i >= string-end)
	    (max 0 (- i 1))
	    (let ((c (text:charAt i)))
	      (match c
		(#\newline
		 (if (is top <= y < (+ top line-height))
		     i
		     (loop (+ i 1) 0 (+ top line-height))))
		(_
		 (let ((width (text-width (text:subSequence i (+ i 1))
					  font)))
		   (if (and (is top <= y < (+ top line-height))
			    (is left <= x < (+ left width)))
		       i
		       (loop (+ i 1) (+ left width) top))))))))))
  
  (define (atom-character-index-under x::real y::real
				      text::CharSequence)
    ::int
    (text-character-index-under x y text (the-atom-font)))
  
  (define (quoted-text-extent text::CharSequence)::Extent
    (text-extent text (the-string-font)))

  (define (quoted-text-character-index-under x::real y::real
					     text::CharSequence)
    ::int
    (text-character-index-under x y text (the-string-font)))

  (define (onDraw c::Canvas)::void
    (set! canvas c)
    (clear!)
    (invoke (the-top-panel) 'draw! '())
    (invoke (current-message-handler)
	    'display-messages canvas))

  (AndroidView source)
  (setFocusable #t)
  (setFocusableInTouchMode #t)
  (paint:setFlags Paint:ANTI_ALIAS_FLAG)
  ;;(setClickable #t)
  (set! activity source))

(define-interface Polysensoric
    (GestureDetector:OnGestureListener
     GestureDetector:OnDoubleTapListener
     #;SensorListener))

(define-object (GRASP)::Polysensoric

  (define view :: View)
  (define gesture-detector ::GestureDetector)
  
  (define (invalidating result::boolean)::boolean
    (when result
      (view:invalidate))
    result)
  
  #|
  (define (onAccuracyChanged sensor::int accuracy::int)::void
    (values))
ue
  (define (onSensorChanged sensor::int values ::float[])::void
    (values))
  |#

  (define (onDown event::MotionEvent)::boolean
    #f)

  (define (onFling e1::MotionEvent e2::MotionEvent
		   vx::float vy::float)
    ::boolean
    #f)

  (define (onLongPress event::MotionEvent)::void
    (view:invalidate))
  
  (define (onScroll e1::MotionEvent e2::MotionEvent
		    dx::float dy::float)
    ::boolean
    #f)

  (define (onShowPress event::MotionEvent)::void
    (values))

  (define (onSingleTapUp event::MotionEvent)::boolean
    #f)

  (define (onSingleTapConfirmed event::MotionEvent)::boolean
    (invalidating
     (invoke (the-top-panel) 'touch!
	     (event:getX)
	     (- (event:getY) 60)
	     0)))

  (define (onDoubleTap event::MotionEvent)::boolean
    #f)

  (define (onDoubleTapEvent event::MotionEvent)::boolean
    #f)

  (define (onDoubleTapConfirmed event::MotionEvent)::boolean
    #f)
  
  (define (onTouchEvent event::MotionEvent)::boolean
    (invalidating
     (or (gesture-detector:onTouchEvent event)
	 (match (event:getActionMasked)
	   (,MotionEvent:ACTION_DOWN
	    ;;(view:showKeyboard)
	    #f)
	   (,MotionEvent:ACTION_POINTER_DOWN
	    #f)
	   (,MotionEvent:ACTION_UP
	    #f)
	   (,MotionEvent:ACTION_POINTER_UP
	    #f)
	   (,MotionEvent:ACTION_OUTSIDE
	    #f)
	   (,MotionEvent:ACTION_MOVE
	    ;;(WARN "force: "(event:getPressure)", size: "(event:getSize))
	    #t)
	   (,MotionEvent:ACTION_CANCEL
	    #f)
	   (_
	    #f)))))

  (define (onKeyUp keyCode::int event::KeyEvent)::boolean
    (parameterize ((ctrl-pressed? (event:ctrl-pressed?))
		   (alt-pressed? (event:alt-pressed?))
		   (shift-pressed? (event:shift-pressed?))
		   (meta-pressed? (event:meta-pressed?)))
      (invalidating
       (invoke (the-top-panel) 'key-pressed!
	       keyCode))))

  (define (onKeyDown keyCode::int event::KeyEvent)::boolean
    (parameterize ((ctrl-pressed? (event:ctrl-pressed?))
		   (alt-pressed? (event:alt-pressed?))
		   (shift-pressed? (event:shift-pressed?))
		   (meta-pressed? (event:meta-pressed?)))
      (let* ((result ::boolean (invoke (the-top-panel) 'key-released!
				       keyCode))
	     (unicode (event:getUnicodeChar (event:getMetaState))))
	(invalidating
	 (or (and (isnt unicode = 0)
		  (invoke (the-top-panel) 'key-typed! unicode))
	     result)))))
  
  (define (onCreate savedState::Bundle)::void
    (invoke-special AndroidActivity (this) 'onCreate savedState)
    (initialize-activity (this))
    (set! gesture-detector (GestureDetector (this) (this)))
    (set! view (View (this)))

    (let* ((screen-extent ::Extent (the-screen-extent))
	   (resources ::AndroidResources (getResources))
	   (metrics ::DisplayMetrics
		    (resources:getDisplayMetrics)))
      (set! screen-extent:width metrics:widthPixels)
      (set! screen-extent:height metrics:heightPixels))

    (set! (current-message-handler) (ScreenLogger 100))
    (setContentView view)
    (set! (the-painter) view)

    (WARN "hello,")
    (WARN "world")
    
    (set! (on-key-press KeyEvent:KEYCODE_DPAD_LEFT)
      (lambda _
	(move-cursor-left!
	 selection: (if (shift-pressed?)
			Selection:resize
			Selection:discard))))

    (set! (on-key-press KeyEvent:KEYCODE_DPAD_RIGHT)
      (lambda _
	(move-cursor-right!
	 selection: (if (shift-pressed?)
			Selection:resize
			Selection:discard))))

    (set! (on-key-press KeyEvent:KEYCODE_DPAD_UP)
      move-cursor-up!)

    (set! (on-key-press KeyEvent:KEYCODE_DPAD_DOWN)
      move-cursor-down!)

    (when (is (the-top-panel) instance? Editor)
      (let ((editor ::Editor (as Editor (the-top-panel))))
	(set! editor:document
	  (with-input-from-string "\
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
" parse-document))))

    )
  
  (AndroidActivity))
