(module-name grasp-desktop)
(module-compile-options main: #t)

(import (srfi :11))
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
(import (input))
(import (extent))
(import (conversions))
(import (parse))


(define-alias Font java.awt.Font)
(define-alias FontMetrics java.awt.FontMetrics)
(define-alias File java.io.File)
(define-alias InputStream java.io.InputStream)
(define ClassLoader ::java.lang.ClassLoader
  (java.lang.ClassLoader:getSystemClassLoader))

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
  (let* ((font-source ::InputStream
		      (ClassLoader:getResourceAsStream path))
	 (font ::Font (Font:createFont
		       Font:TRUETYPE_FONT
		       font-source)))
    (invoke (the-graphics-environment)
	    'registerFont font)
    (font:deriveFont size)))


(define-constant Basic-Regular
  (load-font "assets/Basic-Regular.otf" size: 20))

(define-constant LobsterTwo-Regular
  (load-font "assets/LobsterTwo-Regular.otf" size: 28))

(define-constant Oswald-Regular
  (load-font "assets/Oswald-Regular.ttf" size: 22))

(define-constant GloriaHallelujah
  (load-font "assets/GloriaHallelujah.ttf" size: 16))

(define-constant NotoSerif-Regular
  (load-font "assets/NotoSerif-Regular.ttf"))

(define-parameter+ (the-atom-font) ::Font
  #;Basic-Regular LobsterTwo-Regular)

(define-parameter+ (the-string-font) ::Font
  Basic-Regular #;LobsterTwo-Regular)

(define-parameter+ (the-comment-font) ::Font
  GloriaHallelujah)

(define-parameter (the-cursor-offset)::Position
  (Position left: 0 top: 16))

(define-parameter (the-cursor-extent)::Extent
  (Extent width: 2 height: 16))

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

(define-constant transparent :: Color (Color 0.0 0.0 0.0 0.0))

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
    (let ((graphics (the-graphics-output))
	  (cursor-extent (the-cursor-extent))
	  (cursor-offset (the-cursor-offset)))
      (set! marked-cursor-position:left (+ (current-translation-left)
					   +left))
      (set! marked-cursor-position:top (+ (current-translation-top)
					  +top))
      (graphics:fillRect (+ +left cursor-offset:left)
			 (+ +top cursor-offset:top)
			 cursor-extent:width
			 cursor-extent:height)))
  
  (define (cursor-position)::Position
    marked-cursor-position)

  (define text-color ::Color Color:DARK_GRAY)

  (define background-color ::Color transparent)
  
  (define selection-drawing-mode? ::boolean #f)
  
  (define (enter-selection-drawing-mode!)::void
    (set! selection-drawing-mode? #t)
    (set! text-color Color:WHITE)
    (set! background-color Color:DARK_GRAY))

  (define (exit-selection-drawing-mode!)::void
    (set! selection-drawing-mode? #f)
    (set! text-color Color:DARK_GRAY)
    (set! background-color transparent))
  
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
		      context::Cursor)
    ::void
    (let-values (((selection-start selection-end) (the-selection)))
      (let* ((graphics (the-graphics-output))
	     (focused? (and (pair? (the-cursor))
			    (equal? context (cursor-tail))))
	     (enters-selection-drawing-mode?
	      (and (pair? selection-start)
		   (equal? (tail selection-start) context)))
	     (exits-selection-drawing-mode?
	      (and (pair? selection-end)
		   (equal? (tail selection-end) context)))
	     (metrics ::FontMetrics (graphics:getFontMetrics font))
	     (segment-start 0)
	     (left ::float 0)
	     (lines 1)
	     (height ::float (font:getSize))
	     (string-end (text:length)))
	(parameterize ((the-cursor-extent (Extent width: 2
						  height: height)))
	  (define (render-fragment! segment-end::int)
	    (let* ((fragment (text:subSequence segment-start
					       segment-end))
		   (width (metrics:stringWidth fragment)))
	      (graphics:setColor background-color)
	      (graphics:fillRect left (* (- lines 1) height)
				 width height)
	      (graphics:setColor text-color)
	      (graphics:drawString fragment left (* lines height))
	      (set! left (+ left width))))
	  
	  (graphics:setFont font)
	  (for i from 0 below string-end
	       (when (and focused? (eqv? (head (the-cursor)) i))
		 (render-fragment! i)
		 (set! segment-start i)
		 (mark-cursor! left (* (- lines 1) height)))
	       
	       (when (and enters-selection-drawing-mode?
			  (eqv? (head selection-start) i))
		 (render-fragment! i)
		 (set! segment-start i)
		 (enter-selection-drawing-mode!))
	       
	       (when (and exits-selection-drawing-mode?
			  (eqv? (head selection-end) i))
		 (render-fragment! i)
		 (set! segment-start i)
		 (exit-selection-drawing-mode!))
	       
	       (when (eq? (text:charAt i) #\newline)
		 (render-fragment! i)
		 (set! left 0)
		 (set! lines (+ lines 1))
		 (set! segment-start (+ i 1))))
	  (render-fragment! string-end)
	  (when (and focused? (eqv? (head (the-cursor)) string-end))
	    (mark-cursor! left (* (- lines 1) height)))))))
  
  (define (draw-string! text::CharSequence context::Cursor)::void
    (draw-text! text (the-string-font) context))

  (define quoted-text-cursor-offset::Position
    (Position left: -1 top: 2))
  
  (define (draw-quoted-text! text::CharSequence context::Cursor)::void
    (parameterize ((the-cursor-offset quoted-text-cursor-offset))
      (draw-string! text context)))

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

  (define atom-cursor-offset::Position (Position left: 0 top: 4))

  (define atom-frame-color ::Color (Color #xdddddd))
  
  (define (draw-atom! text::CharSequence context::Cursor)::void
    (let* ((graphics (the-graphics-output))
	   (extent (atom-extent text))
	   (font (the-atom-font)))
      (graphics:setColor atom-frame-color)
      (graphics:fillRoundRect 0 14
			      extent:width (- extent:height 28)
			      12 12)
      (with-translation (4 8)
	  (parameterize ((the-cursor-offset atom-cursor-offset))
	    (draw-text! text font context)))))

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
	   0)
   (invoke (as screen-renderer (the-painter)) 'repaint))

  ((mouseReleased event::MouseEvent)::void
   (values))

  ((mouseDragged event::MouseEvent)::void
   (values))

  ((mouseMoved event::MouseEvent)::void
   (values))
  
  ((keyTyped event::KeyEvent)::void
   ;;(display (event:toString))
   ;;(newline)
   ;;(flush-output-port)
   (parameterize ((ctrl-pressed? (event:control-down?))
		  (alt-pressed? (event:alt-down?))
		  (shift-pressed? (event:shift-down?))
		  (meta-pressed? (event:meta-down?)))
     (invoke (the-top-panel) 'key-typed!
	     (event:getKeyChar))
     (invoke (as screen-renderer (the-painter)) 'repaint)
     (repaint)))

  ((keyReleased event::KeyEvent)::void
   ;;(display (event:toString))
   ;;(newline)
   ;;(flush-output-port)
   (parameterize ((ctrl-pressed? (event:control-down?))
		  (alt-pressed? (event:alt-down?))
		  (shift-pressed? (event:shift-down?))
		  (meta-pressed? (event:meta-down?)))
     (invoke (the-top-panel) 'key-released!
	     (event:getKeyCode))
     (invoke (as screen-renderer (the-painter)) 'repaint)
     (repaint)))

  ((keyPressed event::KeyEvent)::void
   ;;(display (event:toString))
   ;;(newline)
   ;;(flush-output-port)
   (parameterize ((ctrl-pressed? (event:control-down?))
		  (alt-pressed? (event:alt-down?))
		  (shift-pressed? (event:shift-down?))
		  (meta-pressed? (event:meta-down?)))
     (invoke (the-top-panel) 'key-pressed!
	     (event:getKeyCode))
     (invoke (as screen-renderer (the-painter)) 'repaint)
     (repaint)))

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

(set! (on-key-press KeyEvent:VK_LEFT)
      (lambda _
	(move-cursor-left!
	 selection: (if (shift-pressed?)
			Selection:resize
			Selection:discard))))

(set! (on-key-press KeyEvent:VK_RIGHT)
      (lambda _
	(move-cursor-right!
	 selection: (if (shift-pressed?)
			Selection:resize
			Selection:discard))))

(set! (on-key-press KeyEvent:VK_UP)
      move-cursor-up!)

(set! (on-key-press KeyEvent:VK_DOWN)
      move-cursor-down!)

(when (is (the-top-panel) instance? Editor)
      (let ((editor ::Editor (as Editor (the-top-panel))))
    (set! editor:document
      (with-input-from-string "\
(define (! n)
\"Computes the product 1*...*n.
It represents the number of per-
mutations of an n-element set.\"
  (if (<= n 0)
      1
      (* n (! (- n 1))))) 
(e.g. (factorial 5) ===> 120)
(Button action: (lambda () (WARN \"button pressed!\"))
        label: \"Press me!\")
" parse-document))))

(window-screen)
