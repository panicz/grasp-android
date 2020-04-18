package com.GRASP;


import android.graphics.Canvas;
import android.view.View;
import java.util.Deque;
import java.util.ArrayDeque;
import java.util.Iterator;

class Stage extends MultiBox {

    public class ObscuringLayers
	extends ArrayDeque<ObscuringLayer> {
	public boolean remove(Box b) {
	    boolean result = false;
	    Iterator<ObscuringLayer> iter =
		super.iterator();
	    while (iter.hasNext()) {
		ObscuringLayer p = iter.next();
		if (p.box==b) {
		    iter.remove();
		    result = true;
		}
	    }
	    return result;
	}
    };
    
    public ObscuringLayers obscuring =
	new ObscuringLayers();
    //protected Box obscuring = null;
    protected Shape shape = null;
    protected View parentView = null;
    
    @Override
    public void draw(Canvas canvas) {
	super.draw(canvas);
	if (shape != null) {
	    shape.draw(canvas);
	}

	Iterator<ObscuringLayer> layer =
	    obscuring.iterator();

	while (layer.hasNext()) {
	    layer.next().box.draw(canvas);
	}
    }
    
    class RemoveLayer implements TouchHandler {
	ObscuringLayer layer;
	Stage stage;
	public ActionResult action(float x, float y) {
	    stage.obscuring.remove(layer);
	    return ActionProcess;
	}

	public RemoveLayer(ObscuringLayer l, Stage s) {
	    layer = l;
	    stage = s;
	}
    };
    
    ObscuringLayer offTouchRemoves(Box b, Stage s) {
	ObscuringLayer l = new ObscuringLayer(b);
	l.off_touch = new RemoveLayer(l, s);
	return l;
    }

    ObscuringLayer offTouchIgnored(Box b) {
	ObscuringLayer l = new ObscuringLayer(b);
	l.off_touch = positive;
	return l;
    }
    
        
    @Override
    public ActionResult onPress(float x, float y,
				int finger) {
	//GRASP.Log("stage pressed");
	if (!obscuring.isEmpty() && finger == 0) {
	    //GRASP.Log("obscuring is "+obscuring);
	    
	    ObscuringLayer top = obscuring.peekLast();
	    if (top.box.contains(x, y)) {
		return top.box.onPress(x, y, finger);
	    }
	    else {
		//GRASP.Log("reset");
		//obscuring.pollLast();
		top.off_touch.action(x, y);
	    }
	} else if(obscuring.isEmpty()) {
	    ActionResult result =
		super.onPress(x, y, finger);
	    if (result.status ==
		ActionStatus.ReturnedBox) {
		obscuring
		    .addLast(offTouchIgnored(result.box));
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
		Box b = new Flex(shape.left,
				 shape.top,
				 shape.right,
				 shape.bottom);
		addChild(b, x, y);
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
	    Box b = recognize(shape, x, y);

	    obscuring.addLast(offTouchRemoves(b, this));
	    //parentView.invalidate();
	    return ActionProcess;
	}
	else if (!obscuring.isEmpty()) {
	    ObscuringLayer top = obscuring.pollLast();
	    if (top.box.is_embeddable()) {
		addChild(top.box, x, y);
	    }
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

    /*
    @Override
    public ActionResult onUnpress(float x, float y,
				  int finger) {
	if (!obscuring.isEmpty()) {
	    addChild(obscuring.pollLast().box, x, y);
	    return ActionProcess;
	}
	return ActionIgnore;
    }
    */
    
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
		.box
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
	    ObscuringLayer top = obscuring.peekLast();
	    if(top.box.contains(x, y)) {
		if(top.box.onSingleTap(x, y).status
		   == ActionStatus.Ignored) {
		    obscuring.pollLast();
		}
		return ActionProcess;
	    }
	    else {
		return top.off_touch.action(x, y);
	    }
	}
	else {
	    return super.onSingleTap(x, y);
	}
    }

    @Override
    public ActionResult onDoubleTap(float x, float y) {
	if (!obscuring.isEmpty()) {
	    ObscuringLayer top = obscuring.peekLast();
	    if(top.box.contains(x, y)) {
		if (top.box.onDoubleTap(x, y).status
		    == ActionStatus.Ignored) {
		    obscuring.pollLast();
		}
		return ActionProcess;
	    }
	    else {
		return top.off_touch.action(x, y);
	    }
	}
	else {
	    return super.onDoubleTap(x, y);
	}
    }

    @Override
    public ActionResult onHold(float x, float y) {
	ActionResult result;
	if (!obscuring.isEmpty()) {
	    ObscuringLayer top = obscuring.peekLast();
	    if(top.box.contains(x, y)) {
		result = top.box.onHold(x, y);
	    }
	    else {
		result = top.off_touch.action(x, y);
	    }
	}
	else {
	    result = super.onHold(x, y);
	}
	if (result.status
	    == ActionStatus.ReturnedBox) {
	    obscuring
		.addLast(offTouchRemoves(result.box,
					 this));
	    GRASP.Log("stack "+obscuring);
	}
	else if (result.status
		 == ActionStatus.Ignored) {

	}
	
	return result;
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
