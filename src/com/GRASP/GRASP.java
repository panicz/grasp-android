package com.GRASP;

import android.annotation.TargetApi;
import android.app.Activity;
import android.os.Bundle;
import android.os.SystemClock;
import android.content.Context;
import android.view.MotionEvent;
import android.view.View;
import android.view.GestureDetector;
//import android.support.v4.view.GestureDetectorCompat;
import android.view.View.OnTouchListener;
import android.view.View.OnKeyListener;
import android.view.KeyEvent;

import android.widget.TextView;
import android.graphics.Typeface;
import android.graphics.Color;
import android.view.Window;
import android.view.WindowManager;
import java.lang.System;
import java.util.Arrays;


@TargetApi(5)
public class GRASP
    extends Activity
    implements GestureDetector.OnGestureListener,
	       GestureDetector.OnDoubleTapListener,
	       OnKeyListener
	    //   implements OnTouchListener
{
    public static Typeface default_font = null;
    GestureDetector gestureDetector;
    public static Logger log = null;
    static Desktop desktop;
    static long start_time_ms = 0;
    long double_tap_ms = 0;

    public static void Log(String s) {
	if (log != null) {
	    log.log((System.currentTimeMillis()
		     -start_time_ms)
		    +": "+s);
	}
	if (desktop != null) {
	    desktop.invalidate();
	}
    }

    boolean[] finger = new boolean[10];
    
    float[] x = new float[10];
    float[] y = new float[10];
    
    @Override
    public boolean onTouchEvent(MotionEvent event) {
	if (this.gestureDetector.onTouchEvent(event)) {
            return true;
	}
     	int action = event.getActionMasked();

	int pointerCount = event.getPointerCount();
	//Log(MotionEvent.actionToString(action));
	switch (action) {
	case MotionEvent.ACTION_DOWN:	    
	case MotionEvent.ACTION_POINTER_DOWN: {
	    int i = event.getActionIndex();
	    int p = event.getPointerId(i);
	    if (!finger[p]) {
		finger[p] = true;
		desktop.stage.onPress(event.getX(i),
				      event.getY(i),
				      p);
	    }
	    break;
	}
	    
	case MotionEvent.ACTION_UP:
	case MotionEvent.ACTION_POINTER_UP:
	case MotionEvent.ACTION_OUTSIDE: {
	    int i = event.getActionIndex();
	    int p = event.getPointerId(i);

	    if (finger[p]) {
		finger[p] = false;
		desktop.stage.onRelease(event.getX(i),
					event.getY(i),
					p);
	    }
	    break;
	}
	case MotionEvent.ACTION_CANCEL:
	    
	    break;
	    
	case MotionEvent.ACTION_MOVE: {
	    int max_finger = -1;
	    for (int i = 0; i < pointerCount; ++i) {
		int p = event.getPointerId(i);
		x[p] = event.getX(i);
		y[p] = event.getY(i);
		if (!finger[p]) {
		    finger[p] = true;
		    desktop.stage.onPress(x[p], y[p], p);
		}
		max_finger = Math.max(max_finger, p);
	    }
	    desktop.stage.onMotion(x, y, finger,
				   max_finger);
	    break;
	}
	default:
	   
	    break;
	}
	return true;
    }

    @Override
    public void onCreate(Bundle savedState) {
        super.onCreate(savedState);
	start_time_ms = System.currentTimeMillis();
	requestWindowFeature(Window
			     .FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager
			     .LayoutParams
			     .FLAG_FULLSCREEN,
			     WindowManager
			     .LayoutParams
			     .FLAG_FULLSCREEN);
	if (default_font == null) {
	    default_font =
		Typeface
		.createFromAsset(getAssets(),
				 "font.ttf");
	}

	if (log == null) {
	    log = new Logger(120, default_font, 12,
			     Color.BLACK);
	}

	desktop = new Desktop(this);

        setContentView(desktop);
	
	gestureDetector = new GestureDetector(this,this);
        // Set the gesture detector as the double tap
        // listener.
        gestureDetector.setOnDoubleTapListener(this);

        desktop.setOnKeyListener(this);
        desktop.setFocusableInTouchMode(true);
        desktop.requestFocus();
	
	for (int i = 0; i < finger.length; ++i) {
	    finger[i] = false;
	}
    }

    @Override
    public boolean onDown(MotionEvent event) {
	if (System.currentTimeMillis() - double_tap_ms
	    > 10) {
	    //Log("onDown: " + event.toString());
	    if (!finger[0]) {
		finger[0] = true;
		desktop.stage.onPress(event.getX(0),
				      event.getY(0),
				      0);
	    }
	}
        return true;
    }

    @Override
    public boolean onFling(MotionEvent event1,
			   MotionEvent event2,
            float velocityX, float velocityY) {
        return false;
    }

    @Override
    public void onLongPress(MotionEvent event) {
	//Log("onLongPress: " + event.toString());
	desktop.stage.onUnpress(event.getX(),
				event.getY(),
				0);
	
	if (desktop.stage.onHold(event.getX(),
				 event.getY()).status
	    == Box.ActionStatus.Ignored) {
	    log.clear();
	    desktop.invalidate();
	}
    }

    @Override
    public boolean onScroll(MotionEvent event1,
			    MotionEvent event2,
			    float distanceX,
			    float distanceY) {
        return false;
    }

    @Override
    public void onShowPress(MotionEvent event) {}

    @Override
    public boolean onSingleTapUp(MotionEvent event) {
	if (finger[0]) {
	    finger[0] = false;
	    desktop.stage.onRelease(event.getX(0),
				    event.getY(0),
				    0);
	}
        return true;
    }

    @Override
    public boolean onDoubleTap(MotionEvent e) {
        //Log("onDoubleTap: " + e.toString());
	desktop.stage.onUnpress(e.getX(),
				e.getY(),
				0);
	
	desktop.stage.onDoubleTap(e.getX(),
				  e.getY());
	double_tap_ms = System.currentTimeMillis();
        return true;
    }

    @Override
    public boolean onDoubleTapEvent(MotionEvent e) {
        return true;
    }

    @Override
    public boolean onSingleTapConfirmed(MotionEvent e) {
	
	// Log("onSingleTapConfirmed: "+ e.toString());
	desktop.stage.onUnpress(e.getX(),
				e.getY(),
				0);
	
	desktop.stage.onSingleTap(e.getX(),
				  e.getY());

        return true;
    }

    @Override
    public boolean onKey(View view,
			 int keyCode,
			 KeyEvent event) {
	switch (event.getAction()) {
        case KeyEvent.ACTION_DOWN:
	    desktop.stage.onKeyDown(event.getKeyCode());
            break;
        case KeyEvent.ACTION_UP:
	    desktop.stage.onKeyUp(event.getKeyCode());
            break;
        }

	return true;
    }
}

