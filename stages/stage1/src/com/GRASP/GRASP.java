package com.GRASP;

import android.annotation.TargetApi;
import android.app.Activity;
import android.os.Bundle;

//import android.graphics.Canvas;

import android.view.MotionEvent;
import android.view.View;
import android.view.GestureDetector;
//import android.support.v4.view.GestureDetectorCompat;
//import android.view.View.OnTouchListener;
import android.view.View.OnKeyListener;
import android.view.KeyEvent;
import android.graphics.Typeface;
import android.graphics.Color;
import android.graphics.Paint;
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
    public static Logger _log = null;
    public Screen edit;
    public static Screen last_known_edit_instance = null;
    public static Paint paint = null;
    
    public static void log(String s) {
	_log.log((System.currentTimeMillis())+": "+s);
	last_known_edit_instance.invalidate();
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

	default_font =
	    Typeface
	    .createFromAsset(getAssets(),
			     "DroidSans.ttf");

	if (paint == null) {
	    paint = new Paint();
	    //;font = f;
	    paint.setTypeface(default_font);
	    paint.setColor(Color.BLACK);
	}

	if (_log == null) {
	    _log = new Logger(120);
	}


	edit = new Screen(this);

	last_known_edit_instance = edit;
	
        setContentView(edit);
	
	gestureDetector = new GestureDetector(this,this);
        gestureDetector.setOnDoubleTapListener(this);

        edit.setOnKeyListener(this);
        edit.setFocusableInTouchMode(true);
        edit.requestFocus();
    }

    // ze wzgledu na ograniczenia techniczne, zdarzenia
    // tapniecia (w tym podwojnego) i przytrzymania
    // moga dzialac tylko dla pierwszego palca
    
    @Override
    public boolean onDown(MotionEvent event) {
	if(edit.onDown(event)) {
	    edit.invalidate();
	    return true;
	}
	return false;
    }

    
    @Override
    public boolean onFling(MotionEvent _,
			   MotionEvent event,
			   float vx, float vy) {
	if(edit.onUp(event, vx, vy)) {
	    edit.invalidate();
	    return true;
	}
	return false;
    }

    @Override
    public void onLongPress(MotionEvent event) {
	/* co sie dzieje przy przytrzymaniu?*/
	if(edit.onLongPress(event)) {
	    edit.invalidate();
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
        return false;
    }

    @Override
    public boolean onDoubleTap(MotionEvent e) {
        return false;
    }

    @Override
    public boolean onDoubleTapEvent(MotionEvent e) {
        if(edit.onDoubleTap(e)) {
	    edit.invalidate();
	    return true;
	}
	return false;
    }

    @Override
    public boolean onSingleTapConfirmed(MotionEvent e) {
	if(edit.onSingleTap(e)) {
	    edit.invalidate();
	    return true;
	}
	return false;
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
	    return edit.onDown(event);

	case MotionEvent.ACTION_UP:
	case MotionEvent.ACTION_POINTER_UP:
	case MotionEvent.ACTION_OUTSIDE: {
	    return edit.onUp(event, 0, 0);
	}

	case MotionEvent.ACTION_MOVE:
	    return edit.onMotion(event);

	case MotionEvent.ACTION_CANCEL: 
	default: 
	    log("onTouchEvent("+evt(event)+")");
	    break;
	}
	    
	return true;
    }

    
    @Override
    public boolean onKey(View view,
			 int keyCode,
			 KeyEvent event) {
	
	log("onKey("+view+", "+keyCode+", "+event+")");
	return true;
    }

    
}

