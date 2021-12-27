(import (sweetex))
(import (ekspres))
(use-sweetex)

(define-alias Paint android.graphics.Paint)
(define-alias Font android.graphics.Typeface)
(define-alias Color android.graphics.Color)

(define paint::Paint (Paint flags: Paint:ANTI_ALIAS_FLAG))

(define-interface Transform2D ()
  (x x0::float y0::float)::float
  (y x0::float y0::float)::float
  (un-x xt::float yt::float)::float
  (un-y xt::float yt::float)::float)

(define-type (Shift2D horizontally: float
		      vertically: float)
  implementing Transform2D
  with
  ((x x0::float y0::float)::float
   (- x0 horizontally))
  ((y x0::float y0::float)::float
   (- y0 vertically))
  ((un-x xt::float yt::float)::float
   (+ xt horizontally))
  ((un-y xt::float yt::float)::float
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

(define-type (Press ...)
  extending TouchEvent)

(define-type (Release ...)
  extending TouchEvent)

(define-type (Move ...
		   previous-left: float
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

(define-type (KeyPress ...)
  extending KeyEvent)

(define-type (KeyRelease ...)
  extending KeyEvent)

(define-external-interface Pad ()
  (width)::float
  (height)::float
  (render canvas::Canvas)::void
  (update! event::Event)::boolean)

(define-syntax with-canvas
  (syntax-rules (shifted-by
		 scaled-by
		 rotated-by
		 radians
		 degrees)
    ((_ canvas shifted-by (x y) . rest)
     (let ((dx x)
	   (dy y))
       (invoke canvas 'translate dx dy)
       (with-canvas canvas . rest)
       (invoke canvas 'translate (- dx) (- dy))))

    ((_ canvas . actions)
     (begin . actions))))

(define-enum Selected (None First Second))

(define shift::Shift2D (Shift2D))

(define-syntax with-event
  (syntax-rules (shifted-by)
    ((_ event shifted-by (x y) . rest)
     (let ((x0 shift:horizontally)
	   (y0 shift:vertically))
       (set! shift:horizontally x)
       (set! shift:vertically y)
       (event:transform! shift)
       (let ((result (with-event event . rest)))
	 (event:untransform! shift)
	 (set! shift:horizontally x0)
	 (set! shift:vertically y0)
	 result)))

    ((_ event . actions)
     (begin . actions))))

(define-type (Beside left: Pad
		     right: Pad
		     selected: Selected =: Selected:None)
  implementing Pad
  with 
  ((width)::float
   (cached (+ (width left) (width right))))

  ((height)::float
   (cached (max (height left) (height right))))

  ((render canvas::Canvas)::void
   (render left canvas)
   (with-canvas canvas shifted-by ((width left) 0)
     (render right canvas)))

  ((update! event::Event)::boolean
   (match Event
     ((MotionEvent left: x)
      (cond ((is 0 < x <= (width left))
	     (update! left event))
	    ((is 0 < (- x (width left)) <= (width right))
	     (with-event event shifted-by ((width left) 0)
			 (update! right event)))
	    (_
	     #false)))
     ((KeyEvent)
      (match selected
	(Selected:First
	 (update! left event))
	(Selected:Second
	 (update! right event))
	(_
	 #false)))
     (_
      #false))))

(define-type (Below up: Pad
		    down: Pad
		    selected: Selected =: Selected:None)
  implementing Pad
  with 
  ((width)::float
   (cached (max (width up) (width down))))

  ((height)::float
   (cached (+ (height up) (height down))))

  ((render canvas::Canvas)::void
   (render up canvas)
   (with-canvas canvas shifted-by (0 (height up))
     (render down canvas)))

  ((update! event::Event)::boolean
   (match Event
     ((MotionEvent top: y)
      (cond ((is 0 < y <= (height up))
	     (update! up event))
	    ((is 0 < (- y (height up)) <= (height down))
	     (with-event event shifted-by (0 (height up))
			 (update! down event)))
	    (_
	     #false)))
     ((KeyEvent key: key)
      (match selected
	(Selected:First
	 (update! up event))
	(Selected:Second
	 (update! down event))
	(_
	 #false)))
     (_
      #false))))

(define-type (Over bottom: Pad
		   top: Pad)
  implementing Pad
  with 
  ((width)::float
   (cached (max (width up) (width down))))

  ((height)::float
   (cached (max (height up) (height down))))

  ((render canvas::Canvas)::void
   (render bottom canvas)
   (render top canvas))

  ((update! event::Event)::boolean
   (or (update! top event)
       (update! bottom event))))

  
(define-type (Caption text: String
		      font: Font
		      size: float
		      color: int)
  implementing Pad
  with 
  ((width)::float
   (cached
    (begin
      (paint.setTypeface font)
      (paint.setTextSize size)
      (paint.measureText text))))

  ((height)::float
   size)

  ((render canvas::Canvas)::void
   (paint.setTypeface font)
   (paint.setColor color)
   (paint.setTextSize size)
   (canvas.drawText text 0 size paint))

  ((update! event::Event)::boolean
   (or (update! top event)
       (update! bottom event))))

(define-type (Spacer width: float
		     height: float)
  implementing Pad
  with 
  ((width)::float
   width)

  ((height)::float
   height)

  ((render canvas::Canvas)::void
   (begin))

  ((update! event::Event)::boolean
   #false))


;; Jak musi dzialac cache'owanie? Wezmy dla przyladu
;; (cached (max (width up) (width down)))
;; wartosc tego wyrazenia zalezy od wyrazenia
;; (max (width up) (width down))
;; Wartosc "max" zalezy z kolei od wyrazen
;; (width up) oraz (width down)
;; przy czym wiemy, ze width to akcesor metody
;; i jako taki jest deterministyczny (cache'owalny)
;; zas up i down to recordy.

;; Zasadniczo chodzi o to, ze zmiana wartosci (width up)
;; albo (width down) powinna uniewaznic wartosc cache'u,
;; natomiast uniewaznienie innych wartosci cache'u
;; powinno ewentualnie poinformowac wszystkich
;; obserwatorow

;; Mamy zatem taka propozycje:
;; - jezeli pole odnosi sie do rekordu, to ustawienie
;; tego pola powoduje dodanie biezacego obiektu
;; do listy obserwatorow w tym obiekcie
;; - zmiana wartosci pola w danym obiekcie powoduje
;; uniewaznienie cache'u we wszystkich obserwatorach
;; - ponadto rekordy dzielimy na niemutowalne
;; i mutowalne, i powyzsze dotyczy tylko mutowalnych
;; obiektow



;; Local Variables:
;; eval: (put 'with-canvas 'scheme-indent-function 3)
;; eval: (put 'with-event 'scheme-indent-function 3)
;; End:
