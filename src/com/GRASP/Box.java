package com.GRASP;

import android.view.MotionEvent;
import android.graphics.RectF;
import android.graphics.Canvas;

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

    interface TouchHandler {
	public ActionResult action(float x, float y);
    };

    interface MotionHandler {
	public ActionResult action(float [] x, float [] y,
				   boolean [] fingers,
				   int max_finger);
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
    
    public void draw(Canvas canvas);

    public boolean contains(float x, float y);

    public boolean accepts(Box b, float x, float y);

    public void addChild(Box c, float x, float y);
    
    public void onKeyDown(int key);

    public void onKeyUp(int key);

    public float getWidth();

    public float getHeight();
};
