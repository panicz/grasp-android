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

    @Override
    public void addChild(Box c, float x, float y) {
	//GRASP.Log(""+this+".addChild("+c+","+x+","+y+")");
	
	if (c == this) {
	    GRASP.Log("attempted to add "+c+" to itself");
	    return;
	}
		
	for (Box k : children) {
	    if (k.accepts(c, x-area.left, y-area.top)) {
		if (c instanceof MultiBox
		    && k instanceof MultiBox) {
		    MultiBox m = (MultiBox) c;
		    MultiBox n = (MultiBox) k;
		    m.area.left -= n.area.left;
		    m.area.top -= n.area.top;
		    m.area.right -= n.area.left;
		    m.area.bottom -= n.area.top;
		}
		input_receiver = k;
		k.addChild(c,
			   x-area.left,
			   y-area.top);
		return;
	    }
	}
	input_receiver = c;
	children.add(c);
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
	//GRASP.Log("new "+this+"@"+l+", "+t);
    }

    public MultiBox(float l, float t, Box ... bs) {
	area = new RectF(l, t, l, t);
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
