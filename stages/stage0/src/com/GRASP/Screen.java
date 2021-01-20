package com.GRASP;

import android.content.Context;
import android.graphics.Canvas;
//import android.graphics.RectF;
import android.util.DisplayMetrics;
//import android.view.MotionEvent;
import android.view.View;
//import java.util.ArrayList;
import android.view.inputmethod.InputMethodManager;


class Screen extends View {

    GRASP activity;

    float width;
    float height;
    
    public Screen(GRASP source) {
	super(source);

	activity = source;
	DisplayMetrics metrics =
	    source
	    .getResources()
	    .getDisplayMetrics();

	width = (float) metrics.widthPixels;
	height = (float) metrics.heightPixels;	
    }

    public void showKeyboard() {
	if (requestFocus()) {
	    InputMethodManager imm = (InputMethodManager)
                activity
		.getSystemService(Context
				  .INPUT_METHOD_SERVICE);
	    imm.showSoftInput(this,
			      InputMethodManager
			      .SHOW_IMPLICIT);
	}
    }
    
    @Override
    protected void onDraw(Canvas canvas) {
	canvas.drawRGB(255, 255, 255);
	if (activity._log != null) {
	    activity._log.draw(canvas, 0, 0);
	}
    }
}
