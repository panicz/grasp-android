package com.GRASP;

import android.annotation.TargetApi;
import android.app.Activity;
import android.os.Bundle;

//import android.content.Context;
import android.view.MotionEvent;
import android.view.View;
import android.view.GestureDetector;
//import android.support.v4.view.GestureDetectorCompat;
//import android.view.View.OnTouchListener;
import android.view.View.OnKeyListener;
import android.view.KeyEvent;
import android.graphics.Typeface;
import android.graphics.Color;
import android.view.Window;
import android.view.WindowManager;
import java.lang.System;
//import java.util.Arrays;

@TargetApi(5)
public class GRASP
    extends Activity
    implements GestureDetector.OnGestureListener,
	       GestureDetector.OnDoubleTapListener,
	       OnKeyListener
	    //   implements OnTouchListener
{
    public Typeface default_font = null;
    GestureDetector gestureDetector;

    View view;    
    public void log(String s) {
	Utils.log(s);
    }

    static String evt(MotionEvent e) {
	return MotionEvent.actionToString(e.getActionMasked());
    }

    @Override
    public void onCreate(Bundle savedState) {
        super.onCreate(savedState);
	requestWindowFeature(Window.FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager.LayoutParams
			     .FLAG_FULLSCREEN,
			     WindowManager.LayoutParams
			     .FLAG_FULLSCREEN);
	Utils.assets = getAssets();
	
	default_font =
	    Typeface
	    .createFromAsset(Utils.assets,
			     "DroidSans.ttf");

	if (Utils.logger == null) {
	    Utils.logger =
		new Logger(120, default_font, 12,
			   Color.BLACK);
	}

	view = new Screen(this);
	Utils.view = view;
	
        setContentView(view);
	
	gestureDetector = new GestureDetector(this,this);
        gestureDetector.setOnDoubleTapListener(this);

        view.setOnKeyListener(this);
        view.setFocusableInTouchMode(true);
        view.requestFocus();
    }

    @Override
    public boolean onDown(MotionEvent event) {
	int i = event.getActionIndex();
	int finger = event.getPointerId(i);
	//log("onDown("+finger+")");
        return true;
    }

    public boolean onMotion(MotionEvent event) {
	//log("onMotion("+event+")");
	return true;
    }
    
    @Override
    public boolean onFling(MotionEvent event1,
			   MotionEvent event2,
			   float vx, float vy) {
	/*log("onFling("+evt(event1)+", "+evt(event2)+", "
	  +vx+", "+vy+")");*/
        return true;
    }

    @Override
    public void onLongPress(MotionEvent event) {
	//log("onLongPress("+event+")");
    }

    @Override
    public boolean onScroll(MotionEvent event1,
			    MotionEvent event2,
			    float distanceX,
			    float distanceY) {
	/*log("onScroll("+evt(event1)+", "+evt(event2)+", "
	  +distanceX+", "+distanceY+")");*/
        return false;
    }

    @Override
    public void onShowPress(MotionEvent event) {}

    @Override
    public boolean onSingleTapUp(MotionEvent event) {
        return false;
    }

    @Override
    public boolean onDoubleTap(MotionEvent e) {
        return false;
    }

    @Override
    public boolean onDoubleTapEvent(MotionEvent e) {
	//log("onDoubleTapEvent("+evt(e)+")");
        return true;
    }

    @Override
    public boolean onSingleTapConfirmed(MotionEvent e) {
	//log("onSingleTapConfirmed("+evt(e)+")");
	return true;
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
	if (this.gestureDetector.onTouchEvent(event)) {
            return true;
	}
	int action = event.getActionMasked();
	switch(action) {
	case MotionEvent.ACTION_DOWN:	    
	case MotionEvent.ACTION_POINTER_DOWN: 
	    return onDown(event);

	case MotionEvent.ACTION_UP:
	case MotionEvent.ACTION_POINTER_UP:
	case MotionEvent.ACTION_OUTSIDE: {
	    return onFling(event, event, 0, 0);
	}

	case MotionEvent.ACTION_MOVE:
	    return onMotion(event);

	case MotionEvent.ACTION_CANCEL: 
	default: 
	    //log("onTouchEvent("+evt(event)+")");
	    break;
	}
	    
	return true;
    }
    
    @Override
    public boolean onKey(View view,
			 int keyCode,
			 KeyEvent event) {
	
	//log("onKey("+view+", "+keyCode+", "+event+")");
	return true;
    }
}

