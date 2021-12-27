(import (define-syntax-rule))

(define-alias MotionEvent android.view.MotionEvent)
(define-alias View android.view.View)
(define-alias Canvas android.graphics.Canvas)

;; no to pomysl jest taki, ze mamy sobie 'globalnie widoczne'
;; funkcje do renderowania i obslugi zdarzen, i ze one sa
;; wywolywane w metodach 

(define-syntax-rule (initialize-application <app-name>)
  (define-simple-class AppView (android.view.View)
    ((onDraw canvas::Canvas)::void
     (render canvas))
    )
  
  (define-simple-class <app-name>
    (android.app.Activity
     ;;android.view.GestureDetector.OnGestureListener
     ;;android.view.GestureDetector.OnDoubleTapListener
     ;;android.view.View.OnKeyListener
     ;;SensorListener
     )
    ((onCreate (savedInstanceState::android.os.Bundle))::void
     (invoke-special android.app.Activity (this)
		     'onCreate savedInstanceState)
     ((this):setContentView (AppView))))
  )
