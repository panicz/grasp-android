(import (define-syntax-rule))
(import (assert))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (define-property))
(import (define-cache))
(import (define-parameter))
(import (default-value))
(import (infix))
(import (match))
(import (functions))
(import (for))
(import (while))
(import (panel))
(import (painter))
(import (print))

(define-alias Font java.awt.Font)
(define-alias File java.io.File)

(define-alias FocusEvent java.awt.event.FocusEvent)
(define-alias KeyEvent java.awt.event.KeyEvent)
(define-alias ComponentEvent java.awt.event.ComponentEvent)

(define-alias MouseListener java.awt.event.MouseListener)
(define-alias MouseMotionListener java.awt.event.MouseMotionListener)
(define-alias MouseEvent java.awt.event.MouseEvent)

(define-alias MouseWheelListener java.awt.event.MouseWheelListener)
(define-alias MouseWheelEvent java.awt.event.MouseWheelEvent)

(define-alias WindowAdapter java.awt.event.WindowAdapter)
(define-alias WindowEvent java.awt.event.WindowEvent)

(define-alias Graphics java.awt.Graphics)
(define-alias Graphics2D java.awt.Graphics2D)
(define-alias RenderingHints java.awt.RenderingHints)

(define-alias Rectangle java.awt.Rectangle)
(define-alias AffineTransform java.awt.geom.AffineTransform)

(define-parameter (the-graphics-output) :: Graphics2D)

(define-parameter (the-graphics-environment)
  ::java.awt.GraphicsEnvironment
  (invoke-static
   java.awt.GraphicsEnvironment
   'getLocalGraphicsEnvironment))


#;(define-object (AWT-painter)::Painter
  ...)

(define (load-font path::String)
  (let* ((font-file ::File (File path))
	 (font ::Font (Font:createFont
		       Font:TRUETYPE_FONT
		       font-file)))
    (invoke (the-graphics-environment)
	    'registerFont font)
    (invoke font 'deriveFont (as float 24.0))))

(define-constant Basic-Regular
  (load-font "fonts/Basic-Regular.otf"))

(define-constant LobsterTwo-Regular
  (load-font "fonts/LobsterTwo-Regular.otf"))

(define-constant Oswald-Regular
  (load-font "fonts/Oswald-Regular.ttf"))

(define-constant GloriaHallelujah
  (load-font "fonts/GloriaHallelujah.ttf"))

(define-constant NotoSerif-Regular
  (load-font "fonts/NotoSerif-Regular.ttf"))

(define-parameter (the-atom-font) ::Font
  LobsterTwo-Regular)

(define-parameter (the-string-font) ::Font
  Oswald-Regular)

#;(let ((fonts (invoke (the-graphics-environment)
		     'getAvailableFontFamilyNames)))
  (for font in fonts
    (display font)
    (newline)))

(define-object (window-state)
  (define x :: int 24)
  (define y :: int 24)
  (java.awt.Frame))

(define-object (screen-renderer screen::window-state);::Painter
  (define target :: window-state)

  (define (clip! left::real  top::real
		 width::real height::real)
    ::void
    (invoke (the-graphics-output) 'setClip
	    left top width height))
  
  (define (current-clip-width)::real
    (let ((clip-area ::Rectangle
		     (invoke (the-graphics-output)
			     'getClipBounds)))
      (field clip-area 'width)))
    
  (define (current-clip-height)::real
    (let ((clip-area ::Rectangle
		     (invoke (the-graphics-output)
			     'getClipBounds)))
      (field clip-area 'height)))
  
  (define (current-clip-left)::real
    (let ((clip-area ::Rectangle
		     (invoke (the-graphics-output)
			     'getClipBounds)))
      (field clip-area 'x)))
  
  (define (current-clip-top)::real
    (let ((clip-area ::Rectangle
		     (invoke (the-graphics-output)
			     'getClipBounds)))
      (field clip-area 'y)))

  (define (translate! x::real y::real)::void
    (invoke (the-graphics-output)
	    'translate (as double x) (as double y)))
	    
  (define (current-translation-left)::real
    (let ((transform ::AffineTransform
		     (invoke (the-graphics-output)
			     'getTransform)))
      (invoke transform 'getTranslateX)))
    
  (define (current-translation-top)::real
    (let ((transform ::AffineTransform
		     (invoke (the-graphics-output)
			     'getTransform)))
      (invoke transform 'getTranslateY)))
  
  (define (paint graphics::Graphics)::void
    (invoke-special java.awt.Canvas (this) 'paint graphics)
    (let ((graphics-output ::Graphics2D (as Graphics2D graphics)))
      (parameterize ((the-graphics-output graphics-output))
	;; cf. https://docs.oracle.com/javase/tutorial/2d/advanced/quality.html
	(invoke graphics-output 'setRenderingHints
		(RenderingHints
		 RenderingHints:KEY_TEXT_ANTIALIASING
		 RenderingHints:VALUE_TEXT_ANTIALIAS_ON))
	(invoke graphics-output 'setFont (the-atom-font))
	#;(invoke (the-top-panel) 'draw! '())
	
	(invoke (the-graphics-output)
		'drawString "define" target:x target:y))))

  (java.awt.Canvas)

  (set! target screen))

(define-simple-class window-screen (window-state
				    java.awt.event.KeyListener
				    java.awt.event.FocusListener
				    java.awt.event.ComponentListener
				    java.awt.event.WindowListener)
  (renderer :: screen-renderer)
  
  ((keyTyped event::KeyEvent)::void
   (values))

  ((keyReleased event::KeyEvent)::void
   (values))

  ((keyPressed event::KeyEvent)::void
   (match (event:getKeyCode)
     (,KeyEvent:VK_UP
      (set! y (- y 24)))
     (,KeyEvent:VK_DOWN
      (set! y (+ y 24)))
     (,KeyEvent:VK_LEFT
      (set! x (- x 24)))
     (,KeyEvent:VK_RIGHT
      (set! x (+ x 24)))
     (_
      (values)))
   (renderer:repaint)
   (repaint))

  ((focusGained event::FocusEvent)::void
   (values))
  
  ((focusLost event::FocusEvent)::void
   (invoke (invoke event 'getComponent) 'requestFocus))

  ((componentHidden event::ComponentEvent)::void
   (values))

  ((componentShown event::ComponentEvent)::void
   (values))
  
  ((componentMoved event::ComponentEvent)::void
   (values))

  ((componentResized event::ComponentEvent)::void
   (slot-set! (the-screen-extent) 'width
	      (invoke (this) 'getWidth))
   (slot-set! (the-screen-extent) 'height
	      (invoke (this) 'getHeight)))

  ((windowActivated event::WindowEvent)::void
   (values))

  ((windowClosed event::WindowEvent)::void
   (values))

  ((windowClosing event::WindowEvent)::void
   (dispose))

  ((windowDeactivated event::WindowEvent)::void
   (values))

  ((windowDeiconified event::WindowEvent)::void
   (values))

  ((windowIconified event::WindowEvent)::void
   (values))

  ((windowOpened event::WindowEvent)::void
   (values))
  
  ((*init*)
   (set! renderer (screen-renderer (this)))
   ;;(set! (the-painter) renderer)
   (add renderer)
   (setTitle "GRASP")
   (setSize 640 400)
   (setVisible #t)
   
   (addWindowListener (this))
   
   (addKeyListener (this))

   (addFocusListener (this))

   (addComponentListener (this))))

(window-screen)
