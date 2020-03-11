package com.GRASP;


import android.graphics.Canvas;
import android.view.View;
import java.util.Deque;
import java.util.ArrayDeque;
import java.util.Iterator;


class Stage extends MultiBox {

    protected Deque<Box> obscuring =
	new ArrayDeque<Box>();
    //protected Box obscuring = null;
    protected Shape shape = null;
    protected View parentView = null;
    
    @Override
    public void draw(Canvas canvas) {
	super.draw(canvas);
	if (shape != null) {
	    shape.draw(canvas);
	}

	Iterator<Box> layer = obscuring.iterator();

	while (layer.hasNext()) {
	    layer.next().draw(canvas);
	}
    }
        
    @Override
    public ActionResult onPress(float x, float y,
				int finger) {
	//GRASP.Log("stage pressed");
	if (!obscuring.isEmpty() && finger == 0) {
	    //GRASP.Log("obscuring is "+obscuring);
	    
	    Box top = obscuring.peekLast();
	    if (top.contains(x, y)) {
		return top.onPress(x, y, finger);
	    }
	    else {
		//GRASP.Log("reset");
		obscuring.pollLast();
	    }
	} else {
	    ActionResult result =
		super.onPress(x, y, finger);
	    if (result.status ==
		ActionStatus.ReturnedBox) {
		obscuring.addLast(result.box);
		//GRASP.Log("obscuring = "+obscuring);

		if (children.contains(result.box)) {
		    children.remove(result.box);
		}
		//parentView.invalidate();
	    }
	    else if (finger > 0) {
		shape = null;
	    }
	    else {
		shape = new Shape();
		shape.add(x, y);
		//parentView.invalidate();
	    }
	}
	return ActionProcess;
    }

    class CreateBox implements TouchHandler {
	Stage stage;
	
	public CreateBox(Stage stage) {
	    this.stage = stage;
	}
	
	@Override
	public ActionResult action(float x, float y) {
	    // dodajemy do sceny
	    // nowego multiboxa,
	    if (shape != null) {
		addChild(
		    new Flex(shape.left,
			     shape.top,
			     shape.right,
			     shape.bottom),
		    x, y);
		shape = null;
		obscuring.pollLast();

	    }

	    return ActionProcess;
	}
    };
    TouchHandler createBox;

    class AddShape implements TouchHandler {
	Stage stage;
	public AddShape(Stage stage) {
	    this.stage = stage;
	}

	@Override
	public ActionResult action(float x, float y) {
	    if (stage.shape != null) {

		float left = stage.shape.left;
		float top = stage.shape.top;
		addChild(new
			 ListBox(left, top,
				 stage.shape.toOrigin()),
			 x, y);
		
		stage.shape = null;
		stage.obscuring.pollLast();
		//stage.parentView.invalidate();
	    }
	    return ActionProcess;
	}
    }

    TouchHandler addShape;

    class CancelShape implements TouchHandler {
	Stage stage;
	public CancelShape(Stage stage) {
	    this.stage = stage;
	}

	@Override
	public ActionResult action(float x, float y) {
	    if (stage.shape != null) {
		stage.shape = null;
		stage.obscuring.pollLast();
		//stage.parentView.invalidate();
	    }
	    return ActionProcess;
	}
    }

    TouchHandler cancelShape;

    
    Box recognize(Shape shape, float x, float y) {
	return new
	    ListBox(x, y,
		    new Button("Box",
			       createBox,
			       positive,
			       positive,
			       caressing),
		    new Button("Shape",
			       addShape,
			       positive,
			       positive,
			       caressing),
		    new Button("Cancel",
			       cancelShape,
			       positive,
			       positive,
			       caressing));
    }

    @Override
    public ActionResult onRelease(float x, float y,
				  int finger) {
	if (shape != null && obscuring.isEmpty()) {
	    obscuring.addLast(recognize(shape, x, y));
	    //parentView.invalidate();
	    return ActionProcess;
	}
	else if (!obscuring.isEmpty()) {
	    //GRASP.Log("release "+obscuring);
	    /*
	    for (Box child : children) {
		if (child.contains(x, y)
		    && child.accepts(obscuring, x, y)) {
		    child.addChild(obscuring, x, y);
		    obscuring = null;
		    return ActionProcess;
		}
		}*/
	    addChild(obscuring.pollLast(), x, y);
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
	    //parentView.invalidate();

	    return ActionProcess;
	}
	else if (!obscuring.isEmpty()) {
	    return obscuring
		.peekLast()
		.onMotion(x, y, finger, max_finger);
	}
	else {
	    /*
	    return super.onMotion(x, y, finger,
				  max_finger);
	    */
	    return ActionIgnore;
	}
    }

    @Override
    public ActionResult onSingleTap(float x, float y) {
	if (!obscuring.isEmpty()) {
	    return obscuring
		.peekLast()
		.onSingleTap(x, y);
	}
	else {
	    return super.onSingleTap(x, y);
	}
    }

    @Override
    public ActionResult onDoubleTap(float x, float y) {
	if (!obscuring.isEmpty()) {
	    return obscuring
		.peekLast()
		.onDoubleTap(x, y);
	}
	else {
	    return super.onDoubleTap(x, y);
	}
    }

    @Override
    public ActionResult onHold(float x, float y) {
	if (!obscuring.isEmpty()) {
	    
	    ActionResult result =
		obscuring.peekLast().onHold(x, y);
	    if (result.status
		== ActionStatus.ReturnedBox) {
		
	    }
	    return result;
	}
	else {
	    return super.onHold(x, y);
	}
    }
    public Stage(View parent,
		 float l, float t, float r, float b) {
	super(l, t, r, b);
	parentView = parent;
	createBox = new CreateBox(this);
	addShape = new AddShape(this);
	cancelShape = new CancelShape(this);

	/*
	children.add(new ListBox(50,300,
				 "first", "second",
				 "third", "fourth",
				 "fifth"));
	
	*/
    }
    
}
