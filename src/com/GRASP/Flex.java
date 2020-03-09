package com.GRASP;


import android.graphics.Canvas;
import android.view.View;


class Flex extends MultiBox {
    // 1. mozliwosc rozciagania (prawy dolny rog)
    // 2. mozliwosc przesuwania  i umieszczania
    //  (lewy gorny rog)
    // 3. mozliwosc zmiany rodzaju pudelka
    // (przytrzymujemy i wybieramy dostepne
    // opcje)
    ActionResult self;
    
    public Flex(float l, float t, float r, float b) {
	super(l, t, r, b);
	self = new ActionResult(this);
    }
    
    float start_x, start_y;
    
    @Override
    public ActionResult onPress(float x, float y,
				int finger) {
	ActionResult result =
	    super.onPress(x, y, finger);

	if (result.status == ActionStatus.Ignored
	    && finger == 0) {
	    start_x = x;
	    start_y = y;
	    return self;
	}
	else if (result.status ==
		 ActionStatus.ReturnedBox) {
	    if(result.box instanceof MultiBox) {
		MultiBox mbox = (MultiBox) result.box;
		mbox.area.right -= area.left;
		mbox.area.left -= area.left;
		mbox.area.bottom -= area.top;
		mbox.area.top -= area.top;
	    }
	    if (children.contains(result.box)) {
		children.remove(result.box);
	    }
	    return result;
	} else {
	    return result;
	}
    }

    public ActionResult onMotion(float [] x, float [] y,
				 boolean [] finger,
				 int max_fingers) {
	if (max_fingers == 0 && finger[0]) {
	    float dx = x[0] - start_x;
	    float dy = y[0] - start_y;
	    area.left += dx;
	    area.right += dx;
	    area.top += dy;
	    area.bottom += dy;
	    start_x = x[0];
	    start_y = y[0];
	    return ActionProcess;
	}
	else {
	    return ActionIgnore;
	}
    }
}
