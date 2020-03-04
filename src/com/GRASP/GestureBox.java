package com.GRASP;


class GestureBox extends IdleBox {
    public Impassive impassive = new Impassive();
    public Untouchable untouchable = new Untouchable();
    public class Insensitive extends GestureHandler {
	public TouchHandler onSingleTap = impassive;
	public TouchHandler onDoubleTap = impassive;
	public TouchHandler onHold = impassive;
	public MotionHandler onMotion = untouchable;  
    };
    public Insensitive insensitive = new Insensitive();
    
    protected GestureHandler gesture = insensitive;

    public GestureBox() {}

    public GestureBox(GestureHandler handler) {
	gesture = handler;
    }
    
    @Override
    public ActionResult onSingleTap(float x, float y) {
	return gesture.onSingleTap.action(x, y);
    }

    @Override
    public ActionResult onDoubleTap(float x, float y) {
	return gesture.onDoubleTap.action(x, y);
    }

    @Override
    public ActionResult onHold(float x, float y) {	
	return gesture.onHold.action(x, y);
    }
    
};
