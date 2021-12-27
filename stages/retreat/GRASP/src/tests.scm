(import (sweetex))

(use-sweetex)

(import (define-interface))
(import (define-type))
(import (match))


(define-interface Transform2D ()
  (x x0 :: float y0 :: float) :: float
  (y x0 :: float y0 :: float) :: float
  (un-x xt :: float yt :: float) :: float
  (un-y xt :: float yt :: float) :: float)


(define-type (Shift2D horizontally: float
		      vertically: float)
  implementing Transform2D
  with
  ((x x0 :: float y0 :: float):: float
   (- x0 horizontally))
  ((y x0 :: float y0 :: float):: float
   (- y0 vertically))
  ((un-x xt :: float yt :: float):: float
   (+ xt horizontally))
  ((un-y xt :: float yt :: float):: float
   (+ yt vertically)))

(define-external-interface Event ()
  (transform! t::Transform2D)::Event
  (untransform! t::Transform2D)::Event)

(define-type (TouchEvent finger: byte
			 left: float
			 top: float)
  implementing Event
  with
  ((transform! t::Transform2D)::Event
   (let ((x (invoke t 'x left top))
	 (y (invoke t 'y left top)))
     (set! left x)
     (set! top y)
     (this)))
  
  ((untransform! t::Transform2D)::Event
   (let ((x (invoke t 'un-x left top))
	 (y (invoke t 'un-y left top)))
     (set! left x)
     (set! top y)
     (this))))


(define-type (Press)
  extending TouchEvent)

(define-type (Release)
  extending TouchEvent)

(define-type (Click)
  extending TouchEvent)

(define-type (DoubleClick)
  extending TouchEvent)

(define-type (Move previous-left: float
		   previous-top: float)
  extending TouchEvent
  with
  ((transform! t::Transform2D)::Event
   (invoke-special TouchEvent 'transform! t)
   (let ((x (invoke t 'x previous-left previous-top))
	 (y (invoke t 'y previous-left previous-top)))
     (set! previous-left x)
     (set! previous-top y)
     (this)))

  ((untransform! t::Transform2D)::Event
   (invoke-special TouchEvent 'untransform! t)
   (let ((x (invoke t 'un-x previous-left previous-top))
	 (y (invoke t 'un-y previous-left previous-top)))
     (set! previous-left x)
     (set! previous-top y)
     (this))))

(define-type (KeyEvent key: int)
  implementing Event
  with
   ((transform! t::Transform2D)::Event
    (this))
   
   ((untransform! t::Transform2D)::Event
    (this)))

(define-type (KeyPress)
  extending KeyEvent)

(define-type (KeyRelease)
  extending KeyEvent)

