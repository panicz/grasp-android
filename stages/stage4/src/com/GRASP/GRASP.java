package com.GRASP;

import android.annotation.TargetApi;
import android.app.Activity;
import android.os.Bundle;
//import android.graphics.Canvas;
import android.util.DisplayMetrics;


import android.view.MotionEvent;
import android.view.View;
import android.view.GestureDetector;
//import android.support.v4.view.GestureDetectorCompat;
//import android.view.View.OnTouchListener;
import android.view.View.OnKeyListener;
import android.view.KeyEvent;
import android.graphics.Typeface;
//import android.graphics.Color;
import android.graphics.Paint;
import android.view.Window;
import android.view.WindowManager;

//import java.lang.System;
//import java.util.Arrays;

@TargetApi(5)
public class GRASP
    extends Activity
    implements GestureDetector.OnGestureListener,
	       GestureDetector.OnDoubleTapListener,
	       OnKeyListener
	    //   implements OnTouchListener
{
    enum ScreenOrientation {
	Horizontal, Vertical
    }

    ScreenOrientation screenOrientation;
    
    public static Typeface symbols_font = null;
    public static Typeface strings_font = null;
    public static Typeface logs_font = null;
    public static Typeface comments_font = null;
    public static Typeface menu_font = null;

    GestureDetector gestureDetector;
    public static Logger _log = null;
    public Screen screen;
    private static Screen last_known_screen_instance = null;
    public static Paint paint = null;

    private Panel other_panel;
    
    public static void log(String s) {
	_log.log(s);
	last_known_screen_instance.invalidate();
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

	if (comments_font == null) {
	    comments_font =
		Typeface
		.createFromAsset(getAssets(),
				 "GloriaHallelujah.ttf");
	}

	if (menu_font == null) {
	    menu_font =
		Typeface
		.createFromAsset(getAssets(),
				 "Basic-Regular.otf");
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

	gestureDetector = new GestureDetector(this, this);
        gestureDetector.setOnDoubleTapListener(this);


	DisplayMetrics metrics =
	    getResources()
	    .getDisplayMetrics();

	if (metrics.widthPixels < metrics.heightPixels) {
	    screenOrientation = ScreenOrientation.Vertical;
	}
	else {
	    screenOrientation = ScreenOrientation.Horizontal;
	}

	//savedState
	Panel content = null;

	if (savedState != null) {
	    Panel horizontalPanel = savedState
		.getParcelable("horizontal_panel");
	    Panel verticalPanel = savedState
		.getParcelable("vertical_panel");
	    
	    if (screenOrientation == ScreenOrientation.Horizontal
		&& horizontalPanel != null) {
		content = horizontalPanel;
		other_panel = verticalPanel;
	    }
	    else if (screenOrientation == ScreenOrientation.Vertical
		     && verticalPanel != null) {
		content = verticalPanel;
		other_panel = horizontalPanel;
	    }
	    else if (horizontalPanel != null) {
		other_panel = horizontalPanel;
	    }
	    else if (verticalPanel != null) {
		other_panel = verticalPanel;
	    }
	}

	if (content == null) {
	    content = new Editor(0, 0,
				 metrics.widthPixels,
				 metrics.heightPixels,
				 Scratch.instance(),
				 new Grab());
	}
	
	screen = new Screen(this, content);
        setContentView(screen);
	
        screen.setOnKeyListener(this);
        screen.setFocusableInTouchMode(true);
        screen.requestFocus();

	last_known_screen_instance = screen;
	log(screenOrientation.toString());
    }

    @Override
    public void onSaveInstanceState(Bundle state) {
	state.putParcelable("horizontal_panel",
			    (screenOrientation
			     == ScreenOrientation.Horizontal)
			    ? screen.panel
			    : other_panel);
	state.putParcelable("vertical_panel",
			    (screenOrientation
			     == ScreenOrientation.Vertical)
			    ? screen.panel
			    : other_panel);
    }
    
    @Override
    protected void onResume() {
	super.onResume();
	screen.animationSystem.prepare();
    }
    
    // ze wzgledu na ograniczenia techniczne, zdarzenia
    // tapniecia (w tym podwojnego) i przytrzymania
    // moga dzialac tylko dla pierwszego palca

    boolean invalidating(boolean result) {
	if (result) {
	    screen.invalidate();
	}
	return result;
    }
     
    @Override
    public boolean onDown(MotionEvent event) {
	return invalidating(screen.onDown(event));
    }

    
    @Override
    public boolean onFling(MotionEvent _,
			   MotionEvent event,
			   float vx, float vy) {
	return invalidating(screen.onUp(event, vx, vy));
    }

    @Override
    public void onLongPress(MotionEvent event) {
	if(screen.onLongPress(event)) {
	    screen.invalidate();
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
        return invalidating(screen.onDoubleTap(e));
    }

    @Override
    public boolean onDoubleTapEvent(MotionEvent e) {
	return false;
    }

    @Override
    public boolean onSingleTapConfirmed(MotionEvent e) {
	return invalidating(screen.onSingleTap(e));
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
	    return invalidating(screen.onDown(event));

	case MotionEvent.ACTION_UP:
	    return invalidating(screen.onUp(event, 0, 0));

	case MotionEvent.ACTION_POINTER_UP:	    
	    return invalidating(screen.onUp(event, 0, 0));

	case MotionEvent.ACTION_OUTSIDE:
	    return invalidating(screen.onUp(event, 0, 0));

	case MotionEvent.ACTION_MOVE:
	    return invalidating(screen.onMotion(event));

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

