package com.GRASP;

import android.graphics.Canvas;
import java.util.ArrayList;
import android.graphics.Color;
import android.view.KeyEvent;

import android.graphics.RectF;

import android.graphics.Paint;


class MultiBox extends GestureBox {
    protected RectF area;
    protected Paint paint = new Paint();
    protected ArrayList<Box> children =
	new ArrayList<Box>();
    protected Box input_receiver = null;
    public float horizontal_margin = 25;
    public float vertical_margin = 25;
    
    protected ActionResult self;
    
    @Override
    public void moveBy(float dx, float dy) {
	area.left += dx;
	area.top += dy;
	area.right += dx;
	area.bottom += dy;
    }

    /*
     *    A.top == B.top
     * && A.left < B.left
     *
     *   +---+   +---+
     *   | A |   | B |
     *   +---+   +---+
     *
     * -----------------
     *
     *    A.top < B.top
     * && A.left < B.left
     *
     *   +---+
     *   | A |   +---+
     *   +---+   | B |
     *           +---+
     *
     * -----------------
     *
     *           +---+
     *   +---+   | B |
     *   | A |   +---+
     *   +---+
     *
     * -----------------
     *
     *  +-------+
     *  |       | +---+
     *  |   A   | | B |
     *  |       | +---+
     *  +-------+
     *
     * -----------------
     *
     *         +-------+
     *  +---+  |       |
     *  | A |  |   B   |
     *  +---+  |       |
     *         +-------+
     *
     */
    
    @Override
    public boolean precedes(Box b) {
	if (b instanceof MultiBox) {
	    MultiBox m = (MultiBox) b;
	    // albo sa na tej samej wysokosci
	    // i ten jest na lewo of tamtego,
	    // albo ten jest powyzej tamtego
	    return area.top < m.area.top
		|| (area.top == m.area.top
		    && area.left < m.area.left);
	}
	return false;
    }

    public void afterElementAdded(Box target, int index) {
	if (target instanceof MultiBox) {
	    MultiBox t = (MultiBox) target;
	    if (area.left + t.area.right
		> area.right - horizontal_margin) {
		area.right = area.left
		    + t.area.right
		    + horizontal_margin;
	    }

	    if (area.top + t.area.bottom
		> area.bottom - vertical_margin) {
		area.bottom = area.top
		    + t.area.bottom
		    + vertical_margin;
	    }

	}
    }
    
    public int addElement(Box element,float x, float y) {
	int i;
	for (i = 0; i < children.size(); ++i) {
	    if (element.precedes(children.get(i))) {
		break;
	    }
	}
	children.add(i, element);
	afterElementAdded(element, i);
	return i;
    }
    
    @Override
    public ActionResult addChild(Box c, float x, float y){
	if (underbox != null
	    && underbox instanceof MultiBox) {
	    MultiBox m = (MultiBox) underbox;
	    if (m.accepts(c, x-area.left, y-area.top)) {
		c.moveBy(-m.area.left, -m.area.top);
		input_receiver = underbox;
		ActionResult result =
		    m.addChild(c,
			       x-area.left,
			       y-area.top);
		afterElementAdded(m,
				  children.indexOf(m));
		return result;
	    }
	}
	
	//if (true || accepts(c, x, y)) {
	input_receiver = c;
	addElement(c, x, y);
	return self;
	    //}

	    //return ActionIgnore;
    }
    
    @Override
    public ActionResult onSingleTap(float x, float y) {
	ActionResult result = ActionIgnore;
	x -= area.left;
	y -= area.top;
	for (Box child : children) {
	    if (child.contains(x, y)) {
		result = child.onSingleTap(x, y);
		if(result.status != ActionStatus.Ignored){
		    //GRASP.Log("tap passed to "+child);
		    break;
		}
	    }
	}
	return result;
    }

    @Override
    public ActionResult onDoubleTap(float x, float y) {
	ActionResult result = ActionIgnore;
	x -= area.left;
	y -= area.top;
	for (Box child : children) {
	    if (child.contains(x, y)) {
		result = child.onDoubleTap(x, y);
		if(result.status != ActionStatus.Ignored){
		    //GRASP.Log("dbtap passed to "+child);
		    break;
		}
	    }
	}
	return result;
    }

    @Override
    public ActionResult onHold(float x, float y) {
	ActionResult result = ActionIgnore;
	x -= area.left;
	y -= area.top;
	for (Box child : children) {
	    if (child.contains(x, y)) {
		result = child.onHold(x, y);
		if(result.status != ActionStatus.Ignored){
		  //GRASP.Log("onhold passed to "+child);
		    break;
		}
	    }
	}
	return result;
    }

    @Override
    public ActionResult onPress(float x, float y,
				int finger) {
	ActionResult result = ActionIgnore;
	x -= area.left;
	y -= area.top;
	for (Box child : children) {
	    if (child.contains(x, y)) {
		result = child.onPress(x, y, finger);
		if(result.status != ActionStatus.Ignored){
		    //GRASP.Log("press passed to "+child);
		    break;
		}
	    }
	}
	return result;
    }


    @Override
    public ActionResult onUnpress(float x, float y,
				  int finger) {
	ActionResult result = ActionIgnore;
	x -= area.left;
	y -= area.top;
	for (Box child : children) {
	    if (child.contains(x, y)) {
		result = child.onUnpress(x, y, finger);
		if(result.status != ActionStatus.Ignored){
		    break;
		}
	    }
	}
	return result;
    }
    
    @Override
    public ActionResult onRelease(float x, float y,
				  int finger) {
	ActionResult result = ActionIgnore; 
	for (Box child : children) {
	    if (child.contains(x, y)) {
		result = child.onRelease(x, y, finger);
		if(result.status != ActionStatus.Ignored){
		    break;
		}
	    }
	}
	return result;
    }

    @Override
    public ActionResult onMotion(float [] x, float [] y,
				 boolean [] finger,
				 int max_finger) {
	ActionResult result = ActionIgnore;
	for (int i = 0; i <= max_finger; ++i) {
	    x[i] -= area.left;
	    y[i] -= area.top;
	}
	
	for (Box child : children) {
	    result = child.onMotion(x, y,
				    finger,
				    max_finger);
	}
	
	for (int i = 0; i <= max_finger; ++i) {
	    x[i] += area.left;
	    y[i] += area.top;
	}
	return result;
    }

    public Box underbox = null;

    public void clearUnderbox() {
	if (underbox != null
	    && underbox instanceof MultiBox) {
	    MultiBox u = (MultiBox) underbox;
	    u.clearUnderbox();
	}
	underbox = null;
    }
    
    public void onDragIn(Box b, float x, float y) {
	// to be overridden by Flex
	//GRASP.Log(b+" is over "+this);

    }

    public void onDragOut(Box b, float x, float y) {
	// to be overridden by Flex
	//GRASP.Log(b+" is no longer over "+this);
    }
    
    @Override
    public ActionResult onDragOver(Box b,
				   float x, float y) {
	x -= area.left;
	y -= area.top;

	if (underbox != null
	    && underbox.contains(x, y)) {
	    return underbox.onDragOver(b, x, y);
	}
	else {
	    if (underbox != null
		&& underbox instanceof MultiBox) {
		MultiBox m = (MultiBox) underbox;
		m.onDragOut(b, x, y);
		underbox = null;
	    }
	 	
	    for (Box child : children) {
		if (child.contains(x, y)) {
		    underbox = child;
		    if (underbox instanceof MultiBox) {
			MultiBox m = (MultiBox) underbox;
			m.onDragIn(b, x, y);
		    }
		    return underbox.onDragOver(b, x, y);
		}
	    }
	}
	return ActionIgnore;
    }


    @Override
    public void draw(Canvas canvas) {
	//canvas.clipRect(area);
	canvas.translate(area.left, area.top);
	
	float l = 0;
	float t = 0;
	float r = area.width();
	float b = area.height();

	paint.setColor(Color.WHITE);
	
	canvas.drawRect(l, t, r, b, paint);

	paint.setColor(Color.BLACK);
	paint.setStrokeWidth(4);

	canvas.drawLine(l, t, r, t, paint);
	canvas.drawLine(r, t, r, b, paint);
	canvas.drawLine(r, b, l, b, paint);
	canvas.drawLine(l, b, l, t, paint);

	for (Box child : children) {
	    child.draw(canvas);
	}
	canvas.translate(-area.left, -area.top);
	//anvas.clipOutRect(area);
    }

    @Override
    public boolean contains(float x, float y) {
	//GRASP.Log("testing "+area+" against "+x+", "+y);
	//return 0 < x && x < area.width()
	//    && 0 < y && y < area.height();
	return area.contains(x, y);
    }

    public MultiBox(float l, float t, float r, float b) {
	area = new RectF(l, t, r, b);
	self = new ActionResult(this);
	//GRASP.Log("new "+this+"@"+l+", "+t);
    }

    public MultiBox(float l, float t, Box ... bs) {
	area = new RectF(l, t, l, t);
	self = new ActionResult(this);
	for (Box b : bs) {
	    children.add(b);
	    area.bottom += b.getHeight();
	    area.right =
		Math.max(area.right,
			 b.getWidth());
	}
    }

    @Override
    public ActionResult onKeyDown(KeyEvent event) {
	if (input_receiver != null) {
	    return input_receiver.onKeyDown(event);
	}
	return ActionIgnore;
    }

    @Override
    public ActionResult onKeyUp(KeyEvent event) {
	if (input_receiver != null) {
	    return input_receiver.onKeyUp(event);
	}
	return ActionIgnore;
    }
    
    @Override
    public float getWidth() {
	return area.width();
    }

    @Override
    public float getHeight() {
	return area.height();
    }

}
