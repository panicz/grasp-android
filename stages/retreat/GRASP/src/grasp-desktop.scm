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
(import (indexable))
(import (painter))
(import (print))


(define-alias Font java.awt.Font)
(define-alias FontMetrics java.awt.FontMetrics)
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
(define-alias Shape java.awt.Shape)
(define-alias Path2D java.awt.geom.Path2D)

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
    (invoke font 'deriveFont (as float 28.0))))

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

(define-syntax-rule (Path (command args ...) ...)
  (let ((path ::Path2D (Path2D:Float)))
    (invoke path 'command (as float args) ...)
    ...
    path))

(define-constant top-left-paren ::Path2D
  (Path
   (moveTo 10 0)
   (quadTo 2.5 0 0 25)
   (lineTo 5 25)
   (quadTo 5 15 10 15)
   (closePath)))

(define-constant top-left-bounds ::Rectangle
  (invoke top-left-paren 'getBounds))

(define-constant bottom-left-paren ::Path2D
  (Path
   (moveTo 10 25)
   (quadTo 2.5 25 0 0)
   (lineTo 5 0)
   (quadTo 5 10 10 10)
   (closePath)))

(define-constant bottom-left-bounds ::Rectangle
  (invoke bottom-left-paren 'getBounds))

(define-constant top-right-paren ::Path2D
  (Path
   (moveTo 0 0)
   (quadTo 7.5 0 10 25)
   (lineTo 5 25)
   (quadTo 5 15 0 15)
   (closePath)))

(define-constant top-right-bounds ::Rectangle
  (invoke top-right-paren 'getBounds))

(define-constant bottom-right-paren ::Path2D
  (Path
   (moveTo 0 25)
   (quadTo 7.5 25 10 0)
   (lineTo 5 0)
   (quadTo 5 10 0 10)
   (closePath)))

(define-constant bottom-right-bounds ::Rectangle
  (invoke bottom-right-paren 'getBounds))

#;(let ((fonts (invoke (the-graphics-environment)
		     'getAvailableFontFamilyNames)))
  (for font in fonts
    (display font)
    (newline)))


(define-object (window-state)
  (define x :: int 24)
  (define y :: int 24)
  (java.awt.Frame))

(define-syntax-rule (with-translation* (x y) . actions)
  ;; to powinno wyleciec jak tylko screen-renderer
  ;; zacznie implementowac interfejs Painter
  (let ((x! x)
        (y! y))
    (invoke (this) 'translate! x! y!)
    (begin . actions)
    (invoke (this) 'translate! (- x!) (- y!))))


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

  (define rendering-hints ::RenderingHints
    (RenderingHints
     RenderingHints:KEY_TEXT_ANTIALIASING
     RenderingHints:VALUE_TEXT_ANTIALIAS_ON))

  (define (open-paren! height::real)::void
    (let ((line-height (max 0 (- height
				 (field top-left-bounds 'height)
				 (field bottom-left-bounds 'height)))))
      (invoke (the-graphics-output) 'fill top-left-paren)
      (invoke (the-graphics-output) 'fillRect
	      0 (field top-left-bounds 'height)
	      5 line-height)
      (with-translation* (0 (+ (field top-left-bounds 'height)
			       line-height))
	(invoke (the-graphics-output) 'fill bottom-left-paren))))

  (define (close-paren! height::real)::void
    (let ((line-height (max 0 (- height
				 (field top-right-bounds 'height)
				 (field bottom-right-bounds
					'height)))))
      (invoke (the-graphics-output) 'fill top-right-paren)
      (invoke (the-graphics-output) 'fillRect
	      (- (field top-right-bounds 'width) 5)
	      (field top-right-bounds 'height) 5 line-height)

      (with-translation* (0 (+ (field top-right-bounds 'height)
			       line-height))
	(invoke (the-graphics-output) 'fill bottom-right-paren))))

  (define (paren-width)::real
    (field top-left-bounds 'width))

  (define (min-line-height)::real
    (invoke (the-atom-font) 'getSize2D))

  (define (draw-rounded-rectangle! width::real height::real)::void
    (invoke (the-graphics-output) 'drawRoundRect
	    0 0 (as int width) (as int height) 5 5))

  (define left ::real 0)
  (define top ::real 0)

  (define (remember-offset! +left::real +top::real)
    ::void
    (set! left (+ (current-translation-left) +left))
    (set! top (+ (current-translation-top) +top)))
  
  (define (draw-horizontal-bar! width::real)::void
    (invoke (the-graphics-output) 'fillRect
	    0 0 width 5))
    
  (define (draw-vertical-bar! height::real)::void
    (invoke (the-graphics-output) 'fillRect
	    0 0 5 height))
    
  (define (vertical-bar-width)::real
    5)
  
  (define (horizontal-bar-height)::real
    5)
  
  (define (remembered-left)::real
    left)
  
  (define (remembered-top)::real
    top)

  (define (draw-atom! text::CharSequence index::Index)::void
    (invoke (the-graphics-output) 'setFont (the-atom-font))
    (invoke (the-graphics-output) 'drawString text 0 0))
  
  #;(define (atom-extent text::CharSequence)::Extent
    
    ...)

  (define (clear!)::void
    (error "The `clear!' method is not implemented for the AWT
screen-renderer, because the screen is cleared 
automatically by the AWT framework."))
  
  (define (paint graphics::Graphics)::void
    (invoke-special java.awt.Canvas (this) 'paint graphics)
    (let ((graphics-output ::Graphics2D (as Graphics2D graphics)))
      (set! (the-graphics-output) graphics-output)
	;; cf. https://docs.oracle.com/javase/tutorial/2d/advanced/quality.html
	(invoke graphics-output 'setRenderingHints
		rendering-hints)
	#;(invoke (the-top-panel) 'draw! '())
	(invoke (the-graphics-output) 'setFont (the-atom-font))

	(open-paren! 160)
	(invoke (the-graphics-output)
		'drawString "define" target:x target:y)
	(with-translation* (100 0)
	  (close-paren! 160))
	))

  (java.awt.Canvas)
  (invoke rendering-hints 'put
	  RenderingHints:KEY_ANTIALIASING
	  RenderingHints:VALUE_ANTIALIAS_ON)
  (invoke rendering-hints 'put
	  RenderingHints:KEY_RENDERING
	  RenderingHints:VALUE_RENDER_QUALITY)
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
      (set! y (- y 1)))
     (,KeyEvent:VK_DOWN
      (set! y (+ y 1)))
     (,KeyEvent:VK_LEFT
      (set! x (- x 1)))
     (,KeyEvent:VK_RIGHT
      (set! x (+ x 1)))
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
