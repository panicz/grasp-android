package com.GRASP;


//import android.graphics.Canvas;
//import android.view.View;


class Flex extends MultiBox {
    // 1. mozliwosc rozciagania (prawy dolny rog)
    // 2. mozliwosc przesuwania  i umieszczania
    //  (lewy gorny rog)
    // 3. mozliwosc zmiany rodzaju pudelka
    // (przytrzymujemy i wybieramy dostepne
    // opcje)
    ActionResult self;
    GestureHandler reaction =
	new GestureHandler(impassive,
			   impassive,
			   impassive,
			   untouchable);
    
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
	    /*
	    if(result.box instanceof MultiBox) {
		MultiBox mbox = (MultiBox) result.box;
		mbox.area.right += area.left;
		mbox.area.left += area.left;
		mbox.area.bottom += area.top;
		mbox.area.top += area.top;
		}*/
	    if (children.contains(result.box)) {
		//GRASP.Log("removing "+result.box
		//	  +" from "+this);
		children.remove(result.box);
		if (input_receiver == result.box) {
		    input_receiver = null;
		}
	    }
	    return result;
	} else {
	    return result;
	}
    }

    public ActionResult onMotion(float [] x, float [] y,
				 boolean [] finger,
				 int max_fingers) {
	ActionResult result =
	    reaction.onMotion.action(x, y, finger,
				     max_fingers);
	if (result.status != ActionStatus.Ignored) {
	    return result;
	}
	
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

    @Override
    public boolean accepts(Box b, float x, float y) {
	return b instanceof Flex
	    && super.contains(x, y);
    }

    Box specialize(Box me, float x, float y) {
	return new
	    ListBox(x, y,
		    new Button("Text",
			       new LogTouch("Text"),
			       positive,
			       positive,
			       caressing),
		    new Button("Button",
			       new LogTouch("Button"),
			       positive,
			       positive,
			       caressing),
		    new Button("Code",
			       new LogTouch("Code"),
			       positive,
			       positive,
			       caressing));
    }

    @Override
    public ActionResult onSingleTap(float x, float y) {
	ActionResult result
	    = reaction.onSingleTap.action(x, y);
	if (result.status == ActionStatus.Ignored) {
	    result = super.onSingleTap(x, y);
	}
	return result;
    }

    @Override
    public ActionResult onDoubleTap(float x, float y) {
	ActionResult result
	    = reaction.onDoubleTap.action(x, y);
	if (result.status == ActionStatus.Ignored) {
	    result = super.onDoubleTap(x, y);
	}
	return result;
    }

    @Override
    public ActionResult onHold(float x, float y) {
	
	ActionResult result
	    = reaction.onHold.action(x, y);
	if (result.status == ActionStatus.Ignored) {
	    result = super.onHold(x, y);
	}
	if (result.status == ActionStatus.Ignored) {
	    //GRASP.Log("hold creates options list");
	    return new ActionResult(specialize(this,
					       x, y));
	}
	else if (result.status ==
		 ActionStatus.ReturnedBox
		 && result.box instanceof MultiBox) {
	    MultiBox m = (MultiBox) result.box;
	    //GRASP.Log("hold passes "+m+" above");
	    m.area.left += area.left;
	    m.area.right += area.left;
	    m.area.top += area.top;
	    m.area.bottom += area.top;
	}
	return result;
    }

    
}
