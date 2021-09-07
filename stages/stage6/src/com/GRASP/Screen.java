package com.GRASP;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.RectF;
import android.graphics.BlurMaskFilter;
import android.util.DisplayMetrics;
import android.view.MotionEvent;
import android.view.View;
import android.widget.Toast;
import java.util.List;
import java.util.ArrayList;
import java.util.Deque;
import java.util.ArrayDeque;
import java.util.Iterator;
//import android.os.SystemClock;


import android.view.inputmethod.InputMethodManager;

class Screen extends View {

    public Panel panel;
    public Panel other_panel;

    GRASP activity;
    AnimationSystem animationSystem;
    
    public float width;
    public float height;

    public static final byte fingers = 10;
    
    boolean[] finger = new boolean[] {
	false, false, false, false, false,
	false, false, false, false, false
    };
    
    float[] x = new float[10];
    float[] y = new float[10];

    public Shape shape = new Shape();
    public Stroke stroke = null;

    public Deque<Tile> overlay = new ArrayDeque<Tile>();

    public Deque<Pad> layers = new ArrayDeque<Pad>();

    public List<Gesture> known_gestures =
	new ArrayList<Gesture>();
    
    BlurMaskFilter blur = new
	BlurMaskFilter(5.0f,
		       BlurMaskFilter.Blur.NORMAL);
    
    void startDrawingShape() {
	stroke = new Stroke();
    }

    void cancelDrawingShape() {
	shape.clear();
	stroke = null;
    }

    boolean isShapeBeingDrawn() {
	return stroke != null;
    }

    boolean finalizeStroke() {
	boolean result = false;
	if (stroke != null
	    && (stroke.rect.width() > 4
		|| stroke.rect.height() > 4)) {
	    shape.add(stroke);
	    result = true;
	}	
	stroke = null;
	return result;
    }
    
    public Screen(GRASP source, Panel content) {
	super(source);

	// blur doesn;t work with hw acceleration :/
	setLayerType(View.LAYER_TYPE_SOFTWARE, GRASP.paint);

	activity = source;

	animationSystem = new AnimationSystem(this);	
	DisplayMetrics metrics =
	    source
	    .getResources()
	    .getDisplayMetrics();

	width = (float) metrics.widthPixels;
	height = (float) metrics.heightPixels;

	panel = content;

	known_gestures.add(HorizontalLineAcrossTheScreen
			   .instance);
	known_gestures.add(VerticalLineAcrossTheScreen
			   .instance);
	known_gestures.add(new BoxGesture(30.0f));
	known_gestures.add(new UnderscoreGesture(30.0f));
	known_gestures.add(new EvalGesture(30.0f));
    }

    Drag [] drag = new Drag[] {
	null, null, null, null, null,
	null, null, null, null, null
    };

    public boolean isOngoingDragAction() {
	for (byte i = 0; i < fingers; ++i) {
	    if (drag[i] != null) {
		return true;
	    }
	}
	return false;
    }

    float double_start_x;
    float double_start_y;
    boolean double_pending = false;
    boolean double_generated = false;
    
    public boolean onDoubleTap(MotionEvent event) {
	cancelDrawingShape();
	
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
	byte p = (byte) event.getPointerId(i);
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

	Pad top = layers.peekLast();
	
	if (top == null && p > 0 && finger[0]
	    && isShapeBeingDrawn()) {
	    float x0 = x[0];
	    float y0 = y[0];
	    cancelDrawingShape();
	    drag[0] = panel.stretchFrom((byte)0, x0, y0);
	}

	
	Drag d = (top == null)
	    ? panel.onPress(this, p, x[p], y[p])
	    : top.onPress(this, p, x[p], y[p]);

	if (d != null) {
	    drag[p] = d;
	    cancelDrawingShape();
	    return true;
	}
		
	if (n == 1 && p == 0
	    && top == null
	    && !isShapeBeingDrawn()) {
	    startDrawingShape();
	    return true;
	}
	
	return true;
    }

    public boolean onMotion(MotionEvent event) {

	int n = event.getPointerCount();
	byte max_finger = -1;

	if (double_pending
	    && !double_generated
	    && n == 1) {
	    float dx = event.getX();
	    float dy = event.getY();
	    if(Math.abs(dx-double_start_x) > 10
	       && Math.abs(dy-double_start_y) > 10) {
		cancelDrawingShape();
		Pad top = layers.peekLast();
		Drag d = (top == null)
		    ? panel.onSecondPress(this, (byte) 0,
					  double_start_x,
					  double_start_y)
		    : top.onSecondPress(this, (byte) 0,
					double_start_x,
					double_start_y);
		double_generated = true;
		drag[0] = d;
	    }
	}
	
	for (int i = 0; i < n; ++i) {
	    byte p = (byte)event.getPointerId(i);
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
	    stroke.add(x[0], y[0]);
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
		Pad top = layers.peekLast();

		if (top == null) {
		    panel.onDoubleClick(this, (byte)0,
					double_start_x,
					double_start_y);
		}
		else {
		    top.onDoubleClick(this, (byte)0,
					double_start_x,
					double_start_y);
		}
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
	
	if (isShapeBeingDrawn() && p == 0
	    && finalizeStroke()) {
	    
	    Iterator<Gesture> it =
		known_gestures.iterator();

	    while(it.hasNext()) {
		Gesture gesture = it.next();
		if (gesture.recognizes(shape, this)) {
		    Toast
			.makeText(activity.
				  getApplicationContext(),
				  gesture.name,
				  Toast.LENGTH_SHORT)
			.show();
		    gesture.perform(shape, this);
		    cancelDrawingShape();
		    return true;
		}
	    }
	    
	    //suggestShapeActions(x[p], y[p]);
	}
	
	return true;
    }
    
    public boolean onSingleTap(MotionEvent e) {
	cancelDrawingShape();
	Pad top = layers.peekLast();

	if (top == null) {
	    panel.onClick(this, (byte)0,
			  e.getX(), e.getY());
	}
	else {
	    top.onClick(this, (byte)0, e.getX(), e.getY());
	}
	return true;
    }

    public boolean onLongPress(MotionEvent event) {
	if (double_pending) {
	    //GRASP.log("double pending");
	    return false;
	}

	float x = event.getX();
	float y = event.getY();
	
	cancelDrawingShape();

	if (drag[0] != null) {
	    //drag[0].drop(this, x, y, 0, 0);
	    return false;
	}

	Pad top = layers.peekLast();
	
	drag[0] = (top == null)
	    ? panel.onHold(this, (byte)0, x, y)
	    : top.onHold(this, (byte)0, x, y);

	if (drag[0] != null
	    && drag[0] instanceof Popup) {
	    layers.add((Popup) drag[0]);
	}
	
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

	Pad top = layers.peekLast();

	if (top != null) {

	    GRASP.paint.setMaskFilter(blur);


	    if (Button.paint != null) {
		Button.paint.setMaskFilter(blur);
	    }

	    // blur filter mask
	}

	
	panel.render(canvas);

	Iterator<Pad> layer = layers.iterator();
	
	while(layer.hasNext()) {
	    Pad item = layer.next();
	    if (item == top) {
		GRASP.paint.setMaskFilter(null);
	
		if (Button.paint != null) {
		    Button.paint.setMaskFilter(null);
		}
		//remove blur filter mask
	    }
	    int a = GRASP.paint.getAlpha();
	    GRASP.paint.setAlpha(64);
	    canvas.drawRect(0, 0, width, height, GRASP.paint);
	    GRASP.paint.setAlpha(a);
	    item.render(canvas);
	}
	
	Iterator<Tile> tile =  overlay.iterator();

	while(tile.hasNext()) {
	    tile.next().render(canvas);
	}

	shape.draw(canvas);
	
	if (stroke != null) {
	    stroke.draw(canvas);
	}
    }

    public void finishResizingPanels(Split s,
				     float vx,
				     float vy) {
	panel = panel.finishResizing(s, vx, vy);
    }

    void closeDocument(Document document) {
	if (panel.closeDocument(document)
	    && (other_panel == null
		|| other_panel.closeDocument(document))) {
	    Document.close(document);
	}
    }

    boolean onKeyDown(int keycode, char unicode, int meta) {
	if(layers.isEmpty()) {
	    return panel.onKeyDown(this, keycode, unicode, meta);
	}
	else {
	    return layers.peekLast().onKeyDown(this, keycode,
					       unicode, meta);
	}
    }

    boolean onKeyUp(int keycode, char unicode, int meta) {
	if (layers.isEmpty()) {
	    return panel.onKeyUp(this, keycode, unicode, meta);
	}
	else {
	    return layers.peekLast().onKeyUp(this, keycode,
					     unicode, meta);
	}
    }

    public void removeLayerWith(Pad target) {
	target.onRemove(this);
	layers.removeLastOccurrence(target);
    }
}
