package com.GRASP;


import android.graphics.Canvas;
import android.view.View;


class Stage extends MultiBox {

    protected Box obscuring = null;
    protected Shape shape = null;
    protected View parentView = null;
    
    @Override
    public void draw(Canvas canvas) {
	super.draw(canvas);
	if (shape != null) {
	    shape.draw(canvas);
	}
	
	if (obscuring != null) {
	    obscuring.draw(canvas);
	}
    }
    
    public Stage(View parent,
		 float l, float t, float r, float b) {
	super(l, t, r, b);
	parentView = parent;
	/*
	children.add(new ListBox(50,300,
				 "first", "second",
				 "third", "fourth",
				 "fifth"));
	*/
	
    }

    @Override
    public ActionResult onPress(float x, float y,
				int finger) {
	ActionResult result =
	    super.onPress(x, y, finger);
	if (result.status != ActionStatus.Ignored) {
	    return result;
	}

	if (finger > 0) {
	    shape = null;
	}
	else if (obscuring != null) {
	    
	}
	else {
	    shape = new Shape();
	    shape.add(x, y);
	    parentView.invalidate();
	}
	return ActionProcess;
    }

    Box recognize(Shape shape) {
	
	
	return null;
    }

    @Override
    public ActionResult onRelease(float x, float y,
				  int finger) {

	if (shape != null) {
	    obscuring = recognize(shape);
	    return ActionProcess;
	}
	else {
	    ActionResult result =
		super.onRelease(x, y, finger);
	    if (result.status != ActionStatus.Ignored) {
		return result;
	    }
	    return ActionProcess;
	}
    }

    @Override
    public ActionResult onMotion(float [] x, float [] y,
				 boolean [] finger,
				 int max_finger) {
	if (shape != null && finger[0]) {
	    shape.add(x[0], y[0]);
	    parentView.invalidate();

	    return ActionProcess;
	}
	else {
	    return super.onMotion(x, y, finger,
				  max_finger);
	}
    }

}
