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
(define-alias Font android.graphics.Typeface)

(define-early-constant paint ::Paint (Paint))

(define (INFO . messages)
  (let ((result ::java.lang.StringBuilder
		(java.lang.StringBuilder)))
    (for message in messages
	 (result:append message))
  (android.util.Log:i "gradp-android" (result:toString))))

(define-object (ScreenLogger size)::MessageHandler
  
  (define (display-messages output::Object)::void
    (let* ((canvas ::Canvas (as Canvas output))
	   (line-height ::float 12.0)
	   (top ::float line-height))
      ;;(paint:setTypeFace )
      (paint:setTextSize line-height)
      (for message in messages
	   (canvas:drawText message 0 top paint)
	   (set! top (+ top line-height)))))

  (logger size))
  
(define-object (View activity::AndroidActivity);::Painter

  (define canvas :: Canvas)
  
  (define (onDraw c::Canvas)::void
      (set! canvas c)
      ;;(set! (the-painter) (this))
      (invoke (the-top-panel) 'draw! '())
      (canvas:drawRGB 255 255 255)
      ;;(INFO "screen-logger:  "screen-logger)
      ;;(invoke (current-message-handler) 'display-messages canvas)
      )
    
  (AndroidView activity))

(define-interface Polysensoric
    (android.view.View:OnKeyListener
     android.view.GestureDetector:OnGestureListener
     android.view.GestureDetector:OnDoubleTapListener
     #;android.hardware.SensorListener))

(define-object (GRASP)::Polysensoric
  (define scheme :: gnu.expr.Language kawa.standard.Scheme:instance)
  (define (onCreate savedState::Bundle)::void
    (invoke-special android.app.Activity (this) 'onCreate savedState)
    #|
    (unless scheme
      (set! scheme (kawa.standard.Scheme)))
    (kawa.standard.Scheme:registerEnvironment)
    (gnu.mapping.Environment:setCurrent (scheme:getEnvironment))
    |#
    (set! (current-message-handler) (ScreenLogger 20))
    (setContentView (View (this))))

  #|
  (define (onAccuracyChanged sensor::int accuracy::int)::void
    (values))

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
    #f)

  (define (onKey view::AndroidView keyCode::int
		 event::KeyEvent)
    ::boolean
    #f)
  
  (AndroidActivity))
