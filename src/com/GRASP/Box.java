package com.GRASP;

//import android.view.MotionEvent;
//import android.graphics.RectF;
import android.graphics.Canvas;
import android.view.KeyEvent;


interface Box {
    public enum ActionStatus {
	Ignored,
	Processed,
	ReturnedBox
    };

    public class ActionResult {
	ActionStatus status;
	public Box box = null;
	public ActionResult(ActionStatus s) {
	    status = s;
	}
	public ActionResult(Box b) {
	    box = b;
	    status = ActionStatus.ReturnedBox;
	}
    };

    public interface TouchHandler {
	public ActionResult action(float x, float y);
    };

    public interface MotionHandler {
	public ActionResult action(float [] x, float [] y,
				   boolean [] fingers,
				   int max_finger);
    };

    public interface DrawingMethod {
	public void draw(Box box, Canvas canvas);
    };
    
    public interface TypeHandler {
	public ActionResult action(KeyEvent event);
    };
    
    public class ObscuringLayer {
	public Box box;
	public TouchHandler off_touch;
	public ObscuringLayer(Box b, TouchHandler h) {
	    box = b;
	    off_touch = h;
	}
	
	public ObscuringLayer(Box b) {
	    box = b;
	}

    };
    
    class LogTouch implements TouchHandler {
	String message;
	LogTouch(String message) {
	    this.message = message;
	}
	public ActionResult action(float x, float y) {
	    GRASP.Log(message+" at ("+(int)x+", "+(int)y);
	    return ActionProcess;
	}
    };
    
    class GestureHandler {
	public TouchHandler onSingleTap;
	public TouchHandler onDoubleTap;
	public TouchHandler onHold;
	public MotionHandler onMotion;
	public GestureHandler(TouchHandler onSingleTap,
			      TouchHandler onDoubleTap,
			      TouchHandler onHold,
			      MotionHandler onMotion) {
	    this.onSingleTap = onSingleTap;
	    this.onDoubleTap = onDoubleTap;
	    this.onHold = onHold;
	    this.onMotion = onMotion;
	}
    };
    
    public static ActionResult ActionIgnore
	= new ActionResult(ActionStatus.Ignored);

    public static ActionResult ActionProcess
	= new ActionResult(ActionStatus.Processed);

    public class Impassive implements TouchHandler {
	@Override
	public ActionResult action(float x, float y) {
	    return ActionIgnore;
	}
    }

    public class Positive implements TouchHandler {
	@Override
	public ActionResult action(float x, float y) {
	    return ActionProcess;
	}
    }

    public class Criticize implements TypeHandler {
	@Override
	public ActionResult action(KeyEvent event) {
	    return ActionIgnore;
	}
    }

    public class Acclaim implements TypeHandler {
	@Override
	public ActionResult action(KeyEvent event) {
	    return ActionProcess;
	}
    }
    
    public class Untouchable implements MotionHandler {
	public ActionResult action(float [] x, float [] y,
				   boolean [] fingers,
				   int max_finger) {
	    return ActionIgnore;
	}
    }

    public class Caressing implements MotionHandler {
	public ActionResult action(float [] x, float [] y,
				   boolean [] fingers,
				   int max_finger) {
	    return ActionProcess;
	}
    }
    
    public ActionResult onSingleTap(float x, float y);
    
    public ActionResult onDoubleTap(float x, float y);
    
    public ActionResult onHold(float x, float y);
    
    public ActionResult onPress(float x, float y,
				int finger);

    public ActionResult onUnpress(float x, float y,
				  int finger);

    
    public ActionResult onRelease(float x, float y,
				  int finger);
    
    public ActionResult onMotion(float [] x, float [] y,
				 boolean [] finger,
				 int max_finger);

    public ActionResult onDragOver(Box b,
				   float x, float y);
				   
    public void draw(Canvas canvas);

    public boolean contains(float x, float y);

    public boolean accepts(Box b, float x, float y);

    public boolean is_embeddable();
    
    public void addChild(Box c, float x, float y);
    
    public ActionResult onKeyDown(KeyEvent event);

    public ActionResult onKeyUp(KeyEvent event);

    public float getWidth();

    public float getHeight();
};
