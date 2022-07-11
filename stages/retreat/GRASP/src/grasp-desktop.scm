(import (define-syntax-rule))
(import (assert))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (define-property))
(import (define-parameter))
(import (default-value))
(import (infix))
(import (match))
(import (functions))
(import (for))
(import (while))

(define-alias Font java.awt.Font)
(define-alias File java.io.File)

(define-alias FocusEvent java.awt.event.FocusEvent)
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

(define-parameter (the-graphics-output) :: Graphics2D)

(define-parameter (the-atom-font) ::Font)

(define-parameter (the-string-font) :: Font)

#;(define-object (AWT-screen)::Screen
  ...)

(define-object (window-state)
  (define x :: int 10)
  (define y :: int 10)
  (java.awt.Frame))

(define-object (screen-renderer screen::window-state)
  (define target :: window-state)
  
  (define (paint graphics::Graphics)::void
    (invoke-special java.awt.Canvas (this) 'paint graphics)
    (parameterize ((the-graphics-output (as Graphics2D graphics)))
      #;(draw-panel! (the-main-panel))
      (invoke (the-graphics-output)
	      'drawString "X" target:x target:y)))

  (java.awt.Canvas)
  (set! target screen))

(define-simple-class window-screen (window-state
				    java.awt.event.KeyListener
				    java.awt.event.FocusListener)
  (renderer :: java.awt.Canvas)
  
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
   (renderer:repaint)
   (repaint))

  ((focusGained event::FocusEvent)::void
   (values))
  
  ((focusLost event::FocusEvent)::void
   (invoke (invoke event 'getComponent) 'requestFocus))
    
  ((*init*)
   (set! renderer (screen-renderer (this)))
   (add renderer)
   (setTitle "GRASP")
   (setSize 640 400)
   (setVisible #t)
   
   (addWindowListener
    (object (java.awt.event.WindowAdapter)
      ((windowClosing event::WindowEvent)::void
       (dispose))))
   
   (addKeyListener (this))

   (addFocusListener (this))))

(let* ((graphics-environment (invoke-static
			      java.awt.GraphicsEnvironment
			     'getLocalGraphicsEnvironment))
       (font-file ::File (File "LobsterTwo-Regular.otf"))
       (font ::Font (Font:createFont Font:TRUETYPE_FONT font-file)))
  (graphics-environment:registerFont font)
  (let ((fonts (invoke graphics-environment
		       'getAvailableFontFamilyNames)))
    (for font in fonts
      (display font)
      (newline))))

(window-screen)
