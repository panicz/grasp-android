(import (define-syntax-rule))
(import (assert))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (infix))
(import (match))
(import (functions))
(import (for))
(import (while))

(define-alias FocusListener java.awt.event.FocusListener)
(define-alias FocusEvent java.awt.event.FocusEvent)

(define-alias KeyListener java.awt.event.KeyListener)
(define-alias KeyEvent java.awt.event.KeyEvent)

(define-alias MouseListener java.awt.event.MouseListener)
(define-alias MouseMotionListener java.awt.event.MouseMotionListener)
(define-alias MouseEvent java.awt.event.MouseEvent)

(define-alias MouseWheelListener java.awt.event.MouseWheelListener)
(define-alias MouseWheelEvent java.awt.event.MouseWheelEvent)

(define-alias WindowAdapter java.awt.event.WindowAdapter)
(define-alias WindowEvent java.awt.event.WindowEvent)

(define-alias Graphics java.awt.Graphics)
(define-alias Graphics2D java.awt.Graphics2D)

(define x :: int 10)
(define y :: int 10)

(define-simple-class window-screen (java.awt.Frame)
  (canvas :: java.awt.Canvas)
  ((*init*)
   (set! canvas
	 (object (java.awt.Canvas)
	   ((paint graphics::Graphics)::void
	    (invoke-special java.awt.Canvas (this)
			    'paint graphics)
	    (let ((graphics ::Graphics2D
			    (as Graphics2D graphics)))
	      ;;(display "redraw")(newline)
	      (graphics:drawString "X" x y)))))
   (add canvas)
   (setTitle "GRASP")
   (setSize 640 400)
   (setVisible #t)
   
   (addWindowListener
    (object (WindowAdapter)
      ((windowClosing event::WindowEvent)::void
       (dispose))))
   
   (addKeyListener
    (object (KeyListener)
     
      ((keyTyped event::KeyEvent)::void
       (values))
      
      ((keyReleased event::KeyEvent)::void
       (values))
     
      ((keyPressed event::KeyEvent)::void
       (match (event:getKeyCode)
	 (,KeyEvent:VK_UP
	  (set! y (- y 10)))
	 (,KeyEvent:VK_DOWN
	  (set! y (+ y 10)))
	 (,KeyEvent:VK_LEFT
	  (set! x (- x 10)))
	 (,KeyEvent:VK_RIGHT
	  (set! x (+ x 10)))
	 (_
	  (values)))
       (canvas:repaint)
       (repaint))
      ))

   (addFocusListener
    (object (FocusListener)
      ((focusGained event::FocusEvent)::void
       (values))
      ((focusLost event::FocusEvent)::void
       (invoke (invoke event 'getComponent)
	       'requestFocus)
       )))
   ))

(window-screen)
