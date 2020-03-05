package com.GRASP;


class GestureBox extends IdleBox {
    public Impassive impassive = new Impassive();
    public Untouchable untouchable = new Untouchable();
    public Positive positive = new Positive();
    public Caressing caressing = new Caressing();
    
    public GestureHandler insensitive =
	new GestureHandler(impassive,
			   impassive,
			   impassive,
			   untouchable);

    public GestureHandler thickskin =
	new GestureHandler(positive,
			   positive,
			   positive,
			   caressing);   


    public GestureHandler verbose =
	new GestureHandler(new LogTouch("tap"),
			   new LogTouch("dbltap"),
			   new LogTouch("hold"),
			   caressing);
    
    protected GestureHandler gesture = insensitive;

    public class Button {
	public String text;
	public GestureHandler action;
	public Button(String text) {
	    this.text = text;
	    this.action = insensitive;
	}
	
	public Button(String text,
		      GestureHandler action) {
	    this.text = text;
	    this.action = action;
	}
	public Button(String text,
		      TouchHandler onSingleTap,
		      TouchHandler onDoubleTap,
		      TouchHandler onHold,
		      MotionHandler onMotion) {
	    this.text = text;
	    this.action =
		new GestureHandler(onSingleTap,
				   onDoubleTap,
				   onHold,
				   onMotion);
	}	
    };
    
    public GestureBox() {}

    public GestureBox(GestureHandler handler) {
	GRASP.Log("setting gesture to "+handler);
	gesture = handler;
    }
    
    @Override
    public ActionResult onSingleTap(float x, float y) {
	GRASP.Log("gesture.onSingleTap = "
		  +gesture.onSingleTap);
	return gesture.onSingleTap.action(x, y);
    }

    @Override
    public ActionResult onDoubleTap(float x, float y) {
	GRASP.Log("gesture.onDoubleTap = "
		  +gesture.onDoubleTap);

	return gesture.onDoubleTap.action(x, y);
    }

    @Override
    public ActionResult onHold(float x, float y) {
	GRASP.Log("gesture.onHold = "
		  +gesture.onHold);

	return gesture.onHold.action(x, y);
    }
    
};
