package com.GRASP;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.RectF;
import android.util.DisplayMetrics;
import android.view.MotionEvent;
import android.view.View;
import java.util.List;
import java.util.ArrayList;
//import java.util.Deque;
//import java.util.ArrayDeque;
import java.util.Iterator;

import android.view.inputmethod.InputMethodManager;

class Screen extends View implements Layers {

    public Panel panel;

    GRASP activity;
    
    public float width;
    public float height;

    static final int fingers = 10;
    
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

	panel = new Editor(0, 0, width, height,
			  null, 0, 0);
    }

    Drag [] drag = new Drag[] {
	null, null, null, null, null,
	null, null, null, null, null
    };
    
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

    
    public boolean onDown(MotionEvent event) {
	int i = event.getActionIndex();
	int p = event.getPointerId(i);
	int n = event.getPointerCount();
	assert(!finger[p]);
	finger[p] = true;

	x[p] = event.getX(i);
	y[p] = event.getY(i);
	
	if (p > 0 && isShapeBeingDrawn()) {
	    cancelDrawingShape();
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
	    GRASP._log.update("("+x[0]+", "+y[0]+")");
	}
	return true;
    }
    
    public boolean onUp(MotionEvent event,
			float vx, float vy) {
	int i = event.getActionIndex();
	int p = event.getPointerId(i);
	assert(finger[p]);
	finger[p] = false;

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
    
    public boolean onDoubleTap(MotionEvent e) {
	/* co sie dzieje przy podwojnym kliknieciu? */
	// jezeli jest na wyrazeniu, wyrazenie zostaje
	// zmaksymalizowane i przypiete do ekranu
	// (chyba ze juz bylo przypiete -- w takim razie
	// przywracamy kamere sprzed przypiecia)
        return true;
    }

    public boolean onSingleTap(MotionEvent e) {
	return false;
    }

    public boolean onLongPress(MotionEvent event) {
	/* co sie dzieje przy przytrzymaniu?*/
	float x = event.getX();
	float y = event.getY();

	cancelDrawingShape();

	return false;
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
	
	panel.render(canvas, 0, 0, width, height);

	for (Shape segment : segments) {
	    segment.draw(canvas);
	}
	
	if (shape != null) {
	    shape.draw(canvas);
	}

    }

    // Layers' methods:
    
    @Override
    public void finishResizingPanels(Split s,
				     float vx,
				     float vy) {
	panel = panel.finishResizing(s, vx, vy);
    }
}
