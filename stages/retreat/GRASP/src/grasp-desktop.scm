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
(import (primitive))
(import (cursor))

(define-alias Font java.awt.Font)
(define-alias FontMetrics java.awt.FontMetrics)
(define-alias File java.io.File)

(define-alias FocusEvent java.awt.event.FocusEvent)
(define-alias KeyEvent java.awt.event.KeyEvent)
(define-alias ComponentEvent java.awt.event.ComponentEvent)

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

(define-alias Color java.awt.Color)

(define-parameter (the-graphics-output) :: Graphics2D)

(define-cache (color rgb) (Color rgb))

(define-parameter (parenthesis-color) :: procedure
  (lambda ()
    ::Color
    (color #xcccccc)))

(define-parameter (the-graphics-environment)
  ::java.awt.GraphicsEnvironment
  (invoke-static
   java.awt.GraphicsEnvironment
   'getLocalGraphicsEnvironment))

(define (load-font path::String #!key (size ::float 12.0))
  (let* ((font-file ::File (File path))
	 (font ::Font (Font:createFont
		       Font:TRUETYPE_FONT
		       font-file)))
    (invoke (the-graphics-environment)
	    'registerFont font)
    (font:deriveFont size)))

(define-constant Basic-Regular
  (load-font "fonts/Basic-Regular.otf" size: 20))

(define-constant LobsterTwo-Regular
  (load-font "fonts/LobsterTwo-Regular.otf" size: 28))

(define-constant Oswald-Regular
  (load-font "fonts/Oswald-Regular.ttf" size: 22))

(define-constant GloriaHallelujah
  (load-font "fonts/GloriaHallelujah.ttf" size: 16))

(define-constant NotoSerif-Regular
  (load-font "fonts/NotoSerif-Regular.ttf"))

(define-parameter (the-atom-font) ::Font
  #;Basic-Regular LobsterTwo-Regular)

(define-parameter (the-atom-text-color) ::Color
  Color:DARK_GRAY)
  
(define-parameter (the-atom-background-color) ::Color
  (Color #xdddddd))

(define-parameter (the-string-font) ::Font
  Basic-Regular #;LobsterTwo-Regular)

(define-parameter (the-string-text-color) ::Color
  Color:DARK_GRAY)

(define-parameter (the-comment-font) ::Font
  GloriaHallelujah)

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
  (top-left-paren:getBounds))

(define-constant bottom-left-paren ::Path2D
  (Path
   (moveTo 10 25)
   (quadTo 2.5 25 0 0)
   (lineTo 5 0)
   (quadTo 5 10 10 10)
   (closePath)))

(define-constant bottom-left-bounds ::Rectangle
  (bottom-left-paren:getBounds))

(define-constant top-right-paren ::Path2D
  (Path
   (moveTo 0 0)
   (quadTo 7.5 0 10 25)
   (lineTo 5 25)
   (quadTo 5 15 0 15)
   (closePath)))

(define-constant top-right-bounds ::Rectangle
  (top-right-paren:getBounds))

(define-constant bottom-right-paren ::Path2D
  (Path
   (moveTo 0 25)
   (quadTo 7.5 25 10 0)
   (lineTo 5 0)
   (quadTo 5 10 0 10)
   (closePath)))

(define-constant bottom-right-bounds ::Rectangle
  (bottom-right-paren:getBounds))

#;(let ((fonts (invoke (the-graphics-environment)
		     'getAvailableFontFamilyNames)))
  (for font in fonts
    (display font)
    (newline)))

(define-object (screen-renderer)::Painter
  (define (clip! left::real  top::real
		 width::real height::real)
    ::void
    (invoke (the-graphics-output) 'setClip
	    left top width height))
  
  (define (current-clip-width)::real
    (let ((clip-area ::Rectangle
		     (invoke (the-graphics-output)
			     'getClipBounds)))
      clip-area:width))
    
  (define (current-clip-height)::real
    (let ((clip-area ::Rectangle
		     (invoke (the-graphics-output)
			     'getClipBounds)))
      clip-area:height))
  
  (define (current-clip-left)::real
    (let ((clip-area ::Rectangle
		     (invoke (the-graphics-output)
			     'getClipBounds)))
      clip-area:x))
  
  (define (current-clip-top)::real
    (let ((clip-area ::Rectangle
		     (invoke (the-graphics-output)
			     'getClipBounds)))
      clip-area:y))

  (define (translate! x::real y::real)::void
    (invoke (the-graphics-output)
	    'translate (as double x) (as double y)))
	    
  (define (current-translation-left)::real
    (let ((transform ::AffineTransform
		     (invoke (the-graphics-output)
			     'getTransform)))
      (transform:getTranslateX)))
    
  (define (current-translation-top)::real
    (let ((transform ::AffineTransform
		     (invoke (the-graphics-output)
			     'getTransform)))
      (transform:getTranslateY)))

  (define rendering-hints ::RenderingHints
    (RenderingHints RenderingHints:KEY_TEXT_ANTIALIASING
		    RenderingHints:VALUE_TEXT_ANTIALIAS_ON))
    
  (define (open-paren! height::real)::void
    (let ((line-height (max 0 (- height
				 top-left-bounds:height
				 bottom-left-bounds:height)))
	  (graphics (the-graphics-output)))
      (graphics:setColor ((parenthesis-color)))
      (graphics:fill top-left-paren)
      (graphics:fillRect 0 top-left-bounds:height
			 5 line-height)
      (with-translation (0 (+ top-left-bounds:height
			      line-height))
	  (graphics:fill bottom-left-paren))))

  (define (close-paren! height::real)::void
    (let ((line-height (max 0 (- height
				 top-right-bounds:height
				 bottom-right-bounds:height)))
	  (graphics (the-graphics-output)))
      (graphics:setColor ((parenthesis-color)))
      (graphics:fill top-right-paren)
      (graphics:fillRect (- top-right-bounds:width 5)
			 top-right-bounds:height 5 line-height)

      (with-translation (0 (+ top-right-bounds:height
			      line-height))
	  (graphics:fill bottom-right-paren))))

  (define (draw-box! width::real height::real
		     context::Cursor)
    ::void
    (open-paren! height)
    (with-translation ((- width (paren-width)) 0)
	(close-paren! height)))
  
  (define (space-width)::real 8)
  
  (define (paren-width)::real
    top-left-bounds:width)

  (define (min-line-height)::real
    (max (invoke (the-atom-font) 'getSize2D)
	 (+ top-left-bounds:height bottom-left-bounds:height)
	 (+ top-right-bounds:height bottom-right-bounds:height)))

  (define (draw-rounded-rectangle! width::real height::real)::void
    (invoke (the-graphics-output) 'drawRoundRect
	    0 0 (as int width) (as int height) 5 5))

  (define marked-cursor-position ::Position
    (Position left: 0
	      top: 0))
  
  (define (mark-cursor! +left::real +top::real)::void
    (set! marked-cursor-position:left (+ (current-translation-left)
					 +left))
    (set! marked-cursor-position:top (+ (current-translation-top)
					+top)))
  (define (cursor-position)::Position
    marked-cursor-position)

  (define selection-drawing-mode? ::boolean #f)
  
  (define (enter-selection-drawing-mode!)::void
    (set! selection-drawing-mode? #t))

  (define (exit-selection-drawing-mode!)::void
    (set! selection-drawing-mode? #f))
  
  (define (in-selection-drawing-mode?)::boolean
    selection-drawing-mode?)
  
  (define (vertical-bar-width)::real
    5)
  
  (define (horizontal-bar-height)::real
    5)
  
  (define (draw-horizontal-bar! width::real)::void
    (invoke (the-graphics-output) 'fillRect
	    0 0 width (horizontal-bar-height)))
    
  (define (draw-vertical-bar! height::real)::void
    (invoke (the-graphics-output) 'fillRect
	    0 0 (vertical-bar-width) height))

  (define (horizontal-line-height)::real
    20)
  
  (define (vertical-line-width)::real
    20)
  
  (define (draw-horizontal-line! top::real)::void
    (invoke (the-graphics-output) 'fillRect
	    (max 0 (current-clip-left)) top
	    (current-clip-width) (horizontal-line-height)))
    
  (define (draw-vertical-line! left::real)::void
    (invoke (the-graphics-output) 'fillRect
	    left (max 0 (current-clip-top)) 
	    (vertical-line-width) (current-clip-height)))
    
  (define (draw-text! text::CharSequence
		      font::Font
		      context::Cursor)::void
    (let* ((graphics (the-graphics-output))
	   (line-start 0)
	   (lines 1)
	   (height (font:getSize))
	   (string-end (text:length)))
      (graphics:setFont font)
      (for i from 0 below string-end
	   (when (eq? (text:charAt i) #\newline)
	     (graphics:drawString (text:subSequence line-start i)
				  0 (* lines height))
	     (set! lines (+ lines 1))
	     (set! line-start (+ i 1))))
      (graphics:drawString (text:subSequence line-start string-end)
			   0 (* lines height))))
  
  (define (draw-string! text::CharSequence context::Cursor)::void
    (draw-text! text (the-string-font) context))

  (define (draw-quoted-text! text::CharSequence context::Cursor)::void
    (invoke (the-graphics-output) 'setColor
	    (the-string-text-color))
    (draw-string! text context))

  (define (text-extent text::CharSequence font::Font)::Extent
    (let* ((graphics (the-graphics-output))
	   (metrics ::FontMetrics (graphics:getFontMetrics font))
	   (line-start 0)
	   (lines 1)
	   (line-height (metrics:getHeight))
	   (max-width 0)
	   (string-end (text:length)))
      (for i from 0 below string-end
	   (when (eq? (text:charAt i) #\newline)
	     (set! max-width
		   (max max-width
			(metrics:stringWidth (text:subSequence
					      line-start i))))
	     (set! lines (+ lines 1))
	     (set! line-start (+ i 1))))
      (set! max-width
	    (max max-width
		 (metrics:stringWidth
		  (text:subSequence line-start string-end))))
      (Extent width: max-width
	      height: (* lines (metrics:getHeight)))))

  (define (text-character-index-under x::real y::real
				      text::CharSequence
				      font::Font)
    ::int
    (let* ((graphics (the-graphics-output))
	   (metrics ::FontMetrics (graphics:getFontMetrics font))
	   (line-height (metrics:getHeight))
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
		 (let ((width (metrics:charWidth c)))
		   (if (and (is top <= y < (+ top line-height))
			    (is left <= x < (+ left width)))
		       i
		       (loop (+ i 1) (+ left width) top))))))))))
  
  (define (atom-extent text::CharSequence)::Extent
    (let ((inner (text-extent text (the-atom-font))))
      (Extent width: (+ inner:width 8)
	      height: (+ inner:height 16))))

  (define (draw-atom! text::CharSequence context::Cursor)::void
    (let* ((graphics (the-graphics-output))
	   (extent (atom-extent text)))
      (graphics:setColor (the-atom-background-color))
      (graphics:fillRoundRect 0 14
			      extent:width (- extent:height 28)
			      12 12)
      (graphics:setColor (the-atom-text-color))
      (with-translation (4 8)
	  (draw-text! text (the-atom-font) context))))

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
  
  (define (clear!)::void
    (error "The `clear!' method is not implemented for the AWT
screen-renderer, because the screen is cleared 
automatically by the AWT framework."))
  
  (define (paint graphics::Graphics)::void
    (invoke-special javax.swing.JComponent (this) 'paint graphics)
    (let ((graphics-output ::Graphics2D (as Graphics2D graphics)))
      (set! (the-graphics-output) graphics-output)
      (set! (the-painter) (this))
      ;; cf. https://docs.oracle.com/javase/tutorial/2d/advanced/quality.html
      (graphics-output:setRenderingHints rendering-hints)
      (invoke (the-top-panel) 'draw! '())))

  (javax.swing.JComponent)
  (rendering-hints:put RenderingHints:KEY_ANTIALIASING
		       RenderingHints:VALUE_ANTIALIAS_ON)
  (rendering-hints:put RenderingHints:KEY_RENDERING
		       RenderingHints:VALUE_RENDER_QUALITY))

(set! (the-painter) (screen-renderer))

(define-simple-class window-screen (javax.swing.JFrame
				    java.awt.event.KeyListener
				    java.awt.event.FocusListener
				    java.awt.event.ComponentListener
				    java.awt.event.WindowListener
				    java.awt.event.MouseMotionListener
				    java.awt.event.MouseListener)
  ((mouseEntered event::MouseEvent)::void
   (values))

  ((mouseExited event::MouseEvent)::void
   (values))

  ((mouseClicked event::MouseEvent)::void
   (values))

  ((mousePressed event::MouseEvent)::void
   (invoke (the-top-panel) 'touch!
	   (event:getX)
	   (- (event:getY) 26)
	   0))

  ((mouseReleased event::MouseEvent)::void
   (values))

  ((mouseDragged event::MouseEvent)::void
   (values))

  ((mouseMoved event::MouseEvent)::void
   (values))
  
  ((keyTyped event::KeyEvent)::void
   (values))

  ((keyReleased event::KeyEvent)::void
   (values))

  ((keyPressed event::KeyEvent)::void
   #;(match (event:getKeyCode)
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
   (invoke (as screen-renderer (the-painter)) 'repaint)
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
   (add (as screen-renderer (the-painter)))
   
   (setTitle "GRASP")
   (setSize 640 400)
   (setVisible #t)
   
   (addWindowListener (this))
   
   (addKeyListener (this))

   (addFocusListener (this))

   (addComponentListener (this))

   (addMouseListener (this))

   (addMouseMotionListener (this))
   ))

(window-screen)
