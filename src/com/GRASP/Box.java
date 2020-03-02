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

    public void onKeyDown(int key);

    public void onKeyUp(int key);

    public float getWidth();

    public float getHeight();
};
