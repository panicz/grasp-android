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
    public static Typeface symbols_font = null;
    public static Typeface strings_font = null;
    public static Typeface logs_font = null;

    GestureDetector gestureDetector;
    public static Logger _log = null;
    public Screen edit;
    public static Screen last_known_edit_instance = null;
    public static Paint paint = null;


    
    public static void log(String s) {
	_log.log(s);
	last_known_edit_instance.invalidate();
    }

    
    static String evt(MotionEvent e) {
	return MotionEvent
	    .actionToString(e.getActionMasked());
    }

    @Override
    public void onCreate(Bundle savedState) {
        super.onCreate(savedState);
	requestWindowFeature(Window.FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager.LayoutParams
			     .FLAG_FULLSCREEN,
			     WindowManager.LayoutParams
			     .FLAG_FULLSCREEN);

	if (symbols_font == null) {
	    symbols_font =
		Typeface
		.createFromAsset(getAssets(),
				 "LobsterTwo-Regular.otf");
	}
	
	if (strings_font == null) {
	    strings_font =
		Typeface
		.createFromAsset(getAssets(),
				 "NotoSerif-Regular.ttf");
	}

	if (logs_font == null) {
	    logs_font =
		Typeface
		.createFromAsset(getAssets(),
				 "Oswald-Regular.ttf");
	}
	
	if (paint == null) {
	    paint = new Paint();
	    //;font = f;
	    //paint.setTypeface(default_font);
	    paint.setColor(0xff555555);
	    paint.setFlags(Paint.ANTI_ALIAS_FLAG);

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

    boolean invalidating(boolean result) {
	if (result) {
	    edit.invalidate();
	}
	return result;
    }
     
    @Override
    public boolean onDown(MotionEvent event) {
	//log("dwn");
	return invalidating(edit.onDown(event));
    }

    
    @Override
    public boolean onFling(MotionEvent _,
			   MotionEvent event,
			   float vx, float vy) {
	//log("fling");
	return invalidating(edit.onUp(event, vx, vy));
    }

    @Override
    public void onLongPress(MotionEvent event) {
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
	//log("2tap");
        return invalidating(edit.onDoubleTap(e));
    }

    @Override
    public boolean onDoubleTapEvent(MotionEvent e) {
	//log("2tape");
	return false;
    }

    @Override
    public boolean onSingleTapConfirmed(MotionEvent e) {
	return invalidating(edit.onSingleTap(e));
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
	    return invalidating(edit.onDown(event));

	case MotionEvent.ACTION_UP:
	    //log("up");
	    return invalidating(edit.onUp(event, 0, 0));

	case MotionEvent.ACTION_POINTER_UP:	    
	    //log("pup");
	    return invalidating(edit.onUp(event, 0, 0));

	case MotionEvent.ACTION_OUTSIDE:
	    //log("up");
	    return invalidating(edit.onUp(event, 0, 0));

	case MotionEvent.ACTION_MOVE:
	    return invalidating(edit.onMotion(event));

	case MotionEvent.ACTION_CANCEL: 
	default: 
	    log("onTouchEvent("+evt(event)+")");
	    break;
	}
	    
	return false;
    }

    
    @Override
    public boolean onKey(View view,
			 int keyCode,
			 KeyEvent event) {
	
	log("onKey("+view+", "+keyCode+", "+event+")");
	return true;
    }

    
}

