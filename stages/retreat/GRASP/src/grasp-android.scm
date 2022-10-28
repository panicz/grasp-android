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

(define-object (View activity::android.app.Activity)
  (define (onDraw canvas::android.graphics.Canvas)::void
    (canvas:drawRGB 255 255 127))
  (android.view.View activity))

(define-object (GRASP) 
  (define (onCreate savedState::android.os.Bundle)::void
    (invoke-special android.app.Activity (this) 'onCreate savedState)
    (setContentView (View (this))))
  (android.app.Activity))
