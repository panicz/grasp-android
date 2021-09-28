package com.GRASP;

import android.annotation.TargetApi;
import android.app.Activity;
import android.os.Bundle;
//import android.graphics.Canvas;
import android.util.DisplayMetrics;

import android.content.res.Configuration;
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
import android.graphics.Bitmap;

import android.view.Window;
import android.view.WindowManager;
import android.content.pm.PackageManager;

import com.caverock.androidsvg.SVG;
import com.caverock.androidsvg.PreserveAspectRatio;
import java.lang.Exception;


import android.hardware.SensorListener;
import android.hardware.SensorManager;
import android.content.Context;
import java.lang.UnsupportedOperationException;
import android.widget.Toast;

//import java.lang.System;
//import java.util.Arrays;

@TargetApi(5)
public class GRASP
    extends Activity
    implements GestureDetector.OnGestureListener,
	       GestureDetector.OnDoubleTapListener,
	       OnKeyListener,
	       SensorListener
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

    public static SVG empty_file_icon = null;
    public static SVG file_icon = null;
    public static SVG directory_icon = null;
    
    GestureDetector gestureDetector;
    public static Logger _log = null;
    public Screen screen;
    private static Screen last_known_screen_instance = null;
    public static Paint paint = null;
    
    public static GRASP instance;

    SensorManager sensorManager;
    
    public static void log(String s) {
	_log.log(s);
	last_known_screen_instance.invalidate();
    }

    static String evt(MotionEvent e) {
	return MotionEvent
	    .actionToString(e.getActionMasked());
    }

    long lastUpdate;
    float last_x = 0, last_y = 0, last_z = 0;
    float vx, vy, vz;
    float last_vx, last_vy, last_vz;
    long vx_time, vy_time, vz_time;
    
    
    @Override
    public void onAccuracyChanged(int sensor,
				  int accuracy) { }
    
    @Override
    public void onSensorChanged(int sensor,
				float[] values) {
	if (sensor == (SensorManager
		       .SENSOR_ACCELEROMETER)) {
	    long curTime = System.currentTimeMillis();

	    long diffTime = (curTime - lastUpdate);
	    if (diffTime == 0) {
		return;
	    }
  
	    vx = (values[SensorManager.DATA_X]
		  - last_x)/diffTime;
	    vy = (values[SensorManager.DATA_Y]
		  - last_y)/diffTime;
	    vz = (values[SensorManager.DATA_Z]
		  - last_z)/diffTime;

	    if (vx <= -0.1 || 0.1 <= vx) {
		if ((curTime - vx_time) < 1000
		    && S.ign(last_vx) != S.ign(vx)
		    && screen.onShakeSideways(vx)) {
		    screen.invalidate();
		}
		last_vx = vx;
		vx_time = curTime;
	    }

	    if (vy <= -0.1 || 0.1 <= vy) {
		if ((curTime - vy_time) < 1000
		    && S.ign(last_vy) != S.ign(vy)
		    && screen.onShakeUpAndDown(vy)) {
		    screen.invalidate();
		}
		last_vy = vy;
		vy_time = curTime;
	    }
 
	    if (vz <= -0.1 || 0.1 <= vz) {
		if ((curTime - vz_time) < 1000
		    && S.ign(last_vz) != S.ign(vz)
		    && screen
		    .onShakeBackAndForth(vz,
					 (curTime
					  - vz_time))) {
		    screen.invalidate();
		}
		last_vz = vz;
		vz_time = curTime;
	    }

	    last_x = values[SensorManager.DATA_X];
	    last_y = values[SensorManager.DATA_Y];
	    last_z = values[SensorManager.DATA_Z];

	    lastUpdate = curTime;
	}

    }

    @Override
    public void onCreate(Bundle savedState) {
        super.onCreate(savedState);
	requestWindowFeature(Window.FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager.LayoutParams
			     .FLAG_FULLSCREEN,
			     WindowManager.LayoutParams
			     .FLAG_FULLSCREEN);

	instance = this;
	
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

	if (_log == null) {
	    _log = new Logger(120);
	}

	PreserveAspectRatio preserve = PreserveAspectRatio.START;
	
	if (empty_file_icon == null) {
	    try {
		empty_file_icon = SVG
		    .getFromAsset(getAssets(),
				  "empty_file.svg");
		empty_file_icon
		    .setDocumentPreserveAspectRatio(preserve);
		empty_file_icon.setDocumentHeight(64);
	    }
	    catch(Exception e) {
		log(e.toString());
	    }
	}
	
	if (file_icon == null) {
	    try {
		file_icon = SVG
		    .getFromAsset(getAssets(),
				  "file.svg");
		file_icon.setDocumentPreserveAspectRatio(preserve);
		file_icon.setDocumentHeight(64);
	    }
	    catch(Exception e) {
		log(e.toString());
	    }
	}

	if (directory_icon == null) {
	    try {
		directory_icon = SVG
		    .getFromAsset(getAssets(),
				  "directory.svg");
		directory_icon
		    .setDocumentPreserveAspectRatio(preserve);
		directory_icon.setDocumentHeight(64);
	    }
	    catch(Exception e) {
		log(e.toString());
	    }
	}
	
	if (paint == null) {
	    paint = new Paint();
	    //;font = f;
	    //paint.setTypeface(default_font);
	    paint.setColor(0xff555555);
	    paint.setFlags(Paint.ANTI_ALIAS_FLAG);

	}

	gestureDetector = new GestureDetector(this, this);
        gestureDetector.setOnDoubleTapListener(this);

	sensorManager = (SensorManager)
	    getSystemService(SENSOR_SERVICE);

	sensorManager
	    .registerListener(this,
			      SensorManager
			      .SENSOR_ACCELEROMETER,
			      SensorManager
			      .SENSOR_DELAY_GAME);

	DisplayMetrics metrics =
	    getResources()
	    .getDisplayMetrics();

	if (metrics.widthPixels < metrics.heightPixels) {
	    screenOrientation =
		ScreenOrientation.Vertical;
	}
	else {
	    screenOrientation =
		ScreenOrientation.Horizontal;
	}

	//savedState
	Panel content = null;
	Panel other_panel = null;
	
	if (savedState != null) {
	    Panel horizontalPanel = savedState
		.getParcelable("horizontal_panel");
	    Panel verticalPanel = savedState
		.getParcelable("vertical_panel");
	    
	    if (screenOrientation == (ScreenOrientation
				      .Horizontal)
		&& horizontalPanel != null) {
		content = horizontalPanel;
		other_panel = verticalPanel;
	    }
	    else if (screenOrientation==(ScreenOrientation
					 .Vertical)
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
	screen.other_panel = other_panel;
        setContentView(screen);
	
        screen.setOnKeyListener(this);
        screen.setFocusableInTouchMode(true);
        screen.requestFocus();

	last_known_screen_instance = screen;
    }

    @Override
    public void onConfigurationChanged(Configuration
				       newConfig) {
	super.onConfigurationChanged(newConfig);

	DisplayMetrics metrics =
	    getResources()
	    .getDisplayMetrics();

	ScreenOrientation newOrientation =
	    (metrics.widthPixels < metrics.heightPixels)
	    ? ScreenOrientation.Vertical
	    : ScreenOrientation.Horizontal;

	if (newOrientation != screenOrientation) {
	    Panel content = screen.other_panel;
	    if (content == null) {
		content = new Editor(0, 0,
				     metrics.widthPixels,
				     metrics.heightPixels,
				     Scratch.instance(),
				     new Grab());
	    }
	    screen.other_panel = screen.panel;
	    screen.panel = content;
	    screenOrientation = newOrientation;
	    screen.width = metrics.widthPixels;
	    screen.height = metrics.heightPixels;
	    screen.layers.clear();
	    screen.invalidate();
	}
    }
    
    @Override
    public void onSaveInstanceState(Bundle state) {
	state.putParcelable("horizontal_panel",
			    (screenOrientation
			     == ScreenOrientation.Horizontal)
			    ? screen.panel
			    : screen.other_panel);
	state.putParcelable("vertical_panel",
			    (screenOrientation
			     == ScreenOrientation.Vertical)
			    ? screen.panel
			    : screen.other_panel);
    }

    PermissionGrantedHandler permissionGranted =
	NoActionOnPermissionGranted.instance;
    
    @Override
    public void onRequestPermissionsResult(int requestCode,
                                           String[] permissions,
                                           int[] grantResults)
    {
	super.onRequestPermissionsResult(requestCode,
                                         permissions,
                                         grantResults);
        if (grantResults.length > 0) {
            if(grantResults[0] == PackageManager.PERMISSION_GRANTED) {
		permissionGranted.onPermissionGranted(requestCode,
						      permissions,
						      grantResults);
		permissionGranted = NoActionOnPermissionGranted.instance;
	    }
	}
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
    public boolean onKey(View view, int keyCode, KeyEvent event) {
	switch (event.getAction()) {
	case KeyEvent.ACTION_DOWN: {
	    int meta = event.getMetaState();
	    return invalidating(screen
				.onKeyDown(event.getKeyCode(),
					   (char)event
					   .getUnicodeChar(meta),
					   meta));
	}
	case KeyEvent.ACTION_UP: {
	    int meta = event.getMetaState();
	    return invalidating(screen
				.onKeyUp(event.getKeyCode(),
					 (char)event
					 .getUnicodeChar(meta),
					 meta));
	}
	case KeyEvent.ACTION_MULTIPLE: {
	    String chars = event.getCharacters();
	    boolean result = false;
	    if (chars != null) {
		int keycode = event.getKeyCode();
		int meta = event.getMetaState();
		for (int i = 0; i < chars.length(); ++i) {
		    result |= screen.onKeyDown(keycode,
					       chars.charAt(i),
					       meta);
		}
	    }
	    return invalidating(result);
	}

	default:
	    log("onKey("+view+", "+keyCode+", "+event+")");
	    return false;
	}
    }

    
}

