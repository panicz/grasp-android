package com.slayer;

import android.annotation.TargetApi;
import android.app.Activity;
import android.os.Bundle;
import android.view.Window;
import android.view.WindowManager;


//import android.widget.Button;
//import android.widget.TextView;

@TargetApi(5)
public class SlayerActivity extends Activity 
				    //implements OnTouchListener
{
    static String str(int x) {
	StringBuilder builder
	    = new StringBuilder();
	builder.append(x);
	return builder.toString();
    }

    static int max(int a, int b) {
	return a > b ? a : b;
    }

    Logger log = new Logger(120);
    
    enum EditingMode {
	Idle,
	DrawingShape,
	ShiftingOrZoomingScreen,
	MovingRectange
    }

    EditingMode editingMode = EditingMode.Idle;

    GraspView view = null;
    
    @Override
    public void onCreate(Bundle savedState) {
        super.onCreate(savedState);
	requestWindowFeature(Window
			     .FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager
			     .LayoutParams
			     .FLAG_FULLSCREEN,
			     WindowManager
			     .LayoutParams
			     .FLAG_FULLSCREEN);
	view = new GraspView(this, log);
	/*
	if (savedState != null) {
	    Object shapes =
		savedState
		.getSerializable("shapes");
	    if (shapes != null) { 
		view.shapes = (ArrayList<Shape>)
		    shapes;
	    }
	}
	if (view.shapes == null) {
	    view.shapes = new ArrayList<Shape>();
	    }*/
        setContentView(view);
    }

    @Override
    protected void
	onSaveInstanceState(Bundle s) {
    // Make sure to call the super method
	/// so that the states of our views
	// are saved
	super.onSaveInstanceState(s);
	// Save our own state now
	//s.putSerializable("shapes", view.shapes);
    }
}
