package com.GRASP;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.RectF;
import android.util.DisplayMetrics;
import android.view.MotionEvent;
import android.view.View;
import java.util.List;
import java.util.ArrayList;
import java.util.Deque;
import java.util.ArrayDeque;
import java.util.Iterator;
import android.os.SystemClock;

import android.view.inputmethod.InputMethodManager;

class Screen extends View {

    public Panel panel;

    GRASP activity;
    
    public float width;
    public float height;

    public static final int fingers = 10;
    
    boolean[] finger = new boolean[] {
	false, false, false, false, false,
	false, false, false, false, false
    };
    
    float[] x = new float[10];
    float[] y = new float[10];

    public List<Shape> segments = new ArrayList<Shape>();
    public Shape shape = null;
    public RectF shape_area = new
	RectF(Float.POSITIVE_INFINITY,
	      Float.POSITIVE_INFINITY,
	      Float.NEGATIVE_INFINITY,
	      Float.NEGATIVE_INFINITY);

    public Deque<Widget> overlay = new ArrayDeque<Widget>();
    
    void startDrawingShape() {
	shape = new Shape();
    }

    void cancelDrawingShape() {
	shape = null;
	segments.clear();
	shape_area.left = Float.POSITIVE_INFINITY;
	shape_area.top = Float.POSITIVE_INFINITY;
	shape_area.right = Float.NEGATIVE_INFINITY;
	shape_area.bottom = Float.NEGATIVE_INFINITY;
    }

    boolean isShapeBeingDrawn() {
	return shape != null;
    }

    void finalizeShapeSegment() {
	if (shape != null
	    && (shape.rect.width() > 4
		|| shape.rect.height() > 4)) {
	    segments.add(shape);
	    if (shape.rect.left < shape_area.left) {
		shape_area.left = shape.rect.left;
	    }
	    if (shape.rect.right > shape_area.right) {
		shape_area.right = shape.rect.right;
	    }
	    if (shape.rect.top < shape_area.top) {
		shape_area.top = shape.rect.top;
	    }
	    if (shape.rect.bottom > shape_area.bottom) {
		shape_area.bottom = shape.rect.bottom;
	    }
	}
	
	shape = null;
    }
    
    public Screen(GRASP source) {
	super(source);

	activity = source;

	DisplayMetrics metrics =
	    source
	    .getResources()
	    .getDisplayMetrics();

	width = (float) metrics.widthPixels;
	height = (float) metrics.heightPixels;

	panel = new
	    Editor(0, 0, width, height, new
		   Document("\n"
			    +"(define (! n)\n"
			    +" (if (= n 0)\n"
			    +"   1\n"
			    +"  (* n (! (- n 1)))))\n\n\n"
			    +"(e.g. (! 5) \u21d2 120)\n\n\n"
			    +"(e.g.\n"
			    +"  (= (! 5)\n"
			    +"  (* 1 2 3 4 5)))\n\n"
			    +"(define (! n)\n"
			    +"  (let ((result 1))\n"
			    +"  (while (is n > 1)\n"
			    +"  (set! result (* n result))\n"
			    +"  (set! n (- n 1))) \n"
			    +"    result))\n\n"
			    +"(define (map f l)\n"
			    +"  (match l\n"
			    +"  (( ) ( ))\n"
			    +"  ((cons h t)\n"
			    +"   (cons (f h) (map f t)))))\n"),
		   0, 0);
    }

    Drag [] drag = new Drag[] {
	null, null, null, null, null,
	null, null, null, null, null
    };

    public boolean isOngoingDragAction() {
	for (int i = 0; i < fingers; ++i) {
	    if (drag[i] != null) {
		return true;
	    }
	}
	return false;
    }
    
    boolean splittedView(RectF rect) {
	/**
	 * a horizontal line makes a VerticalSplit,
	 * while a vertical line makes a HorizontalSplit
	 */
	if (shape.isHorizontalLine()
	    && panel.canBeSplittedVerticallyBy(rect)) {
	    panel = panel.splitVerticallyBy(rect);
	    cancelDrawingShape();
	    return true;
	}
	
	if(shape.isVerticalLine()
	   && panel.canBeSplittedHorizontallyBy(rect)) {
	    panel = panel.splitHorizontallyBy(rect);
	    cancelDrawingShape();
	    return true;
	}

	return false;
    }

    float double_start_x;
    float double_start_y;
    boolean double_pending = false;
    boolean double_generated = false;
    
    public boolean onDoubleTap(MotionEvent event) {
	int i = event.getActionIndex();
	int p = event.getPointerId(i);
	int n = event.getPointerCount();
	assert(n == 1 && p == 0);
	// store the details of the event in order to decide
	// whether to call 'onDoubleClick' or 'onSecondPress'
	double_start_x = event.getX(i);
	double_start_y = event.getY(i);

	double_pending = true;
	double_generated = false;
        return true;
    }
    
    public boolean onDown(MotionEvent event) {
	
	int i = event.getActionIndex();
	int p = event.getPointerId(i);
	int n = event.getPointerCount();
	
	assert(!finger[p]);
	finger[p] = true;
	
	x[p] = event.getX(i);
	y[p] = event.getY(i);

	if (double_pending && n == 1 && p == 0) {
	    // after receiving onDoubleTap event
	    // the onDown event is generated anyway,
	    // but in such circumstances we simply
	    // ignore it
	    return false;
	}
	
	if (p > 0 && finger[0] && isShapeBeingDrawn()) {
	    float x0 = x[0];
	    float y0 = y[0];
	    cancelDrawingShape();
	    drag[0] = panel.at(x0, y0).stretchFrom(0, x0, y0);
	}
	
	Drag d = panel.onPress(this, p, x[p], y[p]);

	if (d != null) {
	    drag[p] = d;
	    cancelDrawingShape();
	    return true;
	}
		
	if (n == 1 && p == 0
	    && !isShapeBeingDrawn()) {
	    startDrawingShape();
	    return true;
	}
	
	return true;
    }

    public boolean onMotion(MotionEvent event) {

	int n = event.getPointerCount();
	int max_finger = -1;

	if (double_pending
	    && !double_generated
	    && n == 1) {
	    float dx = event.getX();
	    float dy = event.getY();
	    if(Math.abs(dx-double_start_x) > 10
	       && Math.abs(dy-double_start_y) > 10) {
		Drag d = panel.onSecondPress(this, 0,
					     double_start_x,
					     double_start_y);
		double_generated = true;
		drag[0] = d;
	    }
	}
	
	for (int i = 0; i < n; ++i) {
	    int p = event.getPointerId(i);
	    float xp = event.getX(i);
	    float yp = event.getY(i);
	    assert(finger[p]);
	    max_finger = (p > max_finger)
		? p : max_finger;

	    if (drag[p] != null) {
		drag[p].move(this, xp, yp, xp-x[p], yp-y[p]);
	    }

	    x[p] = xp;
	    y[p] = yp;
	}
	
	if (max_finger == 0
	    && n == 1
	    && isShapeBeingDrawn()) {
	    shape.add(x[0], y[0]);
	    //GRASP._log.update("("+x[0]+", "+y[0]+")");
	}

	if (Panel.stretches > 0) {
	    panel.stretch();
	}
	
	return true;
    }
    
    public boolean onUp(MotionEvent event,
			float vx, float vy) {
	int i = event.getActionIndex();
	int p = event.getPointerId(i);
	assert(finger[p]);
	finger[p] = false;

	if (p == 0) {
	    if (double_pending && !double_generated) {
		panel.onDoubleClick(this, 0,
				    double_start_x,
				    double_start_y);
		double_generated = true;
		assert(drag[0] == null);
	    }
	    double_pending = false;
	}
	
	if (drag[p] != null) {
	    drag[p].drop(this, x[p], y[p], vx, vy);
	    drag[p] = null;
	    return true;
	}
	
	if (isShapeBeingDrawn() && p == 0) {
	    
	    if (segments.isEmpty()
		&& splittedView(shape.rect)) {	       
		return true;
	    }
	    
	    finalizeShapeSegment();
	    //suggestShapeActions(x[p], y[p]);
	}
	
	return true;
    }
    
    public boolean onSingleTap(MotionEvent e) {
	//GRASP.log("tap("+
	panel.onClick(this, 0, e.getX(), e.getY());
	return true;
    }

    public boolean onLongPress(MotionEvent event) {
	if (double_pending) {
	    return false;
	}

	float x = event.getX();
	float y = event.getY();
	
	cancelDrawingShape();

	panel.onHold(this, 0, x, y);
	//GRASP._log.clear();
	return true;
    } 
    
    public void showKeyboard() {
	if (requestFocus()) {
	    InputMethodManager imm = (InputMethodManager)
		activity
                .getSystemService(Context
				  .INPUT_METHOD_SERVICE);
	    imm.showSoftInput(this,
			      InputMethodManager
			      .SHOW_IMPLICIT);
	}
    }
    
    @Override
    protected void onDraw(Canvas canvas) {
	canvas.drawRGB(255, 255, 255);
	GRASP._log.draw(canvas, 0, 0);
	
	panel.render(canvas);

	Iterator<Widget> it =  overlay.iterator();

	while(it.hasNext()) {
	    it.next().render(canvas);
	}
	
	for (Shape segment : segments) {
	    segment.draw(canvas);
	}
	
	if (shape != null) {
	    shape.draw(canvas);
	}
    }

    public void finishResizingPanels(Split s,
				     float vx,
				     float vy) {
	panel = panel.finishResizing(s, vx, vy);
    }
}
