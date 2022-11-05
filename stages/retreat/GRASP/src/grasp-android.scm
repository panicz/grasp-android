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

(import (panel))
(import (indexable))
(import (painter))
(import (print))
(import (extent))

#|
(import (cursor))
(import (input))
(import (primitive))
|#

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

(define-early-constant paint ::Paint (Paint))

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
    )

(define (INFO . messages)
  (let ((result ::java.lang.StringBuilder
		(java.lang.StringBuilder)))
    (for message in messages
	 (result:append message))
  (android.util.Log:i "grasp-android" (result:toString))))

(define-object (ScreenLogger size)::MessageHandler
  
  (define (display-messages output::Object)::void
    (let* ((canvas ::Canvas (as Canvas output))
	   (font (the-log-font))
	   (top ::float font:size))
      (paint:setTypeface font:face)
      (paint:setTextSize font:size)
      (for message in messages
	   (canvas:drawText message 0 top paint)
	   (set! top (+ top font:size)))))

  (logger size))
  
(define-object (View source::AndroidActivity);::Painter

  (define canvas ::Canvas)

  (define activity ::AndroidActivity)
  
  (define (onDraw c::Canvas)::void
      (set! canvas c)
      (invoke (the-top-panel) 'draw! '())
      (canvas:drawRGB 255 255 255)
      (invoke (current-message-handler)
	      'display-messages canvas)
      )

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
    (canvas:clipRect left top (+ left width) (+ top height))
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
  
  #|
  
  (define (draw-horizontal-line! top::real)::void )
  (define (draw-vertical-line! left::real)::void )
  (define (horizontal-line-height)::real )
  (define (vertical-line-width)::real )
  
  (define (mark-cursor! +left::real +top::real)::void )
  (define (cursor-position)::Position )
  
  (define (space-width)::real )
  
  (define (paren-width)::real )
  (define (min-line-height)::real )
  
  (define (clear!)::void )
  
  (define (draw-quoted-text! s::CharSequence context::Cursor)::void )
  (define (draw-string! s::CharSequence context::Cursor)::void )
  (define (quoted-text-extent text::CharSequence)::Extent )
  
  (define (draw-atom! text::CharSequence context::Cursor)::void )
  
  (define (atom-character-index-under x::real y::real text::CharSequence)::int )
  (define (quoted-text-character-index-under x::real y::real text::CharSequence)::int )
  
  (define (atom-extent text::CharSequence)::Extent )
  
  (define (draw-horizontal-bar! width::real)::void )
  (define (draw-vertical-bar! height::real)::void )
  
  (define (vertical-bar-width)::real )
  (define (horizontal-bar-height)::real )
  
  (define (draw-box! width::real height::real context::Cursor)::void )
  
  (define (draw-rounded-rectangle! width::real height::real)::void )
  
  (define (enter-selection-drawing-mode!)::void )
  (define (exit-selection-drawing-mode!)::void )
  (define (in-selection-drawing-mode?)::boolean )
  |#
  (AndroidView source)
  (setFocusable #t)
  (setFocusableInTouchMode #t)
  ;;(setClickable #t)
  (set! activity source))

(define-interface Polysensoric
    (android.view.View:OnKeyListener
     android.view.GestureDetector:OnGestureListener
     android.view.GestureDetector:OnDoubleTapListener
     #;android.hardware.SensorListener))

(define-object (GRASP)::Polysensoric

  (define view :: View)

  (define (invalidating result::boolean)::boolean
    (when result
      (view:invalidate))
    result)
  
  (define (onCreate savedState::Bundle)::void
    (invoke-special android.app.Activity (this) 'onCreate savedState)
    (initialize-activity (this))
    (set! view (View (this)))
    (set! (current-message-handler) (ScreenLogger 100))
    (WARN "activity initialized")
    (setContentView view)
    ;;(set! (the-painter) view)
    )

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

  (define (onLongPress event::MotionEvent)::boolean
    #f)
  
  (define (onScroll e1::MotionEvent e2::MotionEvent
		    dx::float dy::float)
    ::boolean
    #f)

  (define (onShowPress event::MotionEvent)::void
    (values))

  (define (onSingleTapUp event::MotionEvent)::boolean
    #f)

  (define (onSingleTapConfirmed event::MotionEvent)::boolean
    #f)

  (define (onDoubleTap event::MotionEvent)::boolean
    #f)

  (define (onDoubleTapEvent event::MotionEvent)::boolean
    #f)

  (define (onDoubleTapConfirmed event::MotionEvent)::boolean
    #f)

  (define (onTouchEvent event::MotionEvent)::boolean
    (view:invalidate)
    (match (event:getActionMasked)
      (,MotionEvent:ACTION_DOWN
       (view:showKeyboard)
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
       #f)
      (,MotionEvent:ACTION_CANCEL
       #f)
      (_
       #f)))

  (define (onKey view::AndroidView keyCode::int
		 event::KeyEvent)
    ::boolean
    (match (event:getAction)
      (,KeyEvent:ACTION_UP
       (invalidating
	(invoke (the-top-panel) 'key-released!
		(event:getKeyCode))))
      (,KeyEvent:ACTION_DOWN
       (let* ((result ::boolean (invoke (the-top-panel) 'key-released!
					(event:getKeyCode)))
	      (unicode (event:getUnicodeChar (event:getMetaState))))
	 (invalidating
	  (or (and (isnt unicode = 0)
		   (invoke (the-top-panel) 'key-typed! unicode))
	      result))))
      (_
       #f)))
  
  (AndroidActivity))
