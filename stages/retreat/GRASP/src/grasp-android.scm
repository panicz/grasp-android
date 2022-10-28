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

(define-alias Bundle android.os.Bundle)

(define-alias KeyEvent android.view.KeyEvent)

(define-alias MotionEvent android.view.MotionEvent)

#|
(import (cursor))
(import (input))
(import (primitive))
|#

(define-object (View activity::android.app.Activity)
  (define (onDraw canvas::android.graphics.Canvas)::void
    (canvas:drawRGB 255 255 255))
  (android.view.View activity))

(define-interface Polysensoric
    (android.view.View:OnKeyListener
     android.view.GestureDetector:OnGestureListener
     android.view.GestureDetector:OnDoubleTapListener
     #;android.hardware.SensorListener))

(define-object (GRASP)::Polysensoric
  (define (onCreate savedState::Bundle)::void
    (invoke-special android.app.Activity (this) 'onCreate savedState)
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

  (define (onKey view::android.view.View keyCode::int
		 event::KeyEvent)
    ::boolean
    #f)
  
  (android.app.Activity))
