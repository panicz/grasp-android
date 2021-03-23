package com.GRASP;

import android.content.Context;
import android.graphics.Canvas;
//import android.graphics.RectF;
import android.graphics.Paint;

import android.util.DisplayMetrics;
//import android.view.MotionEvent;
import android.view.View;
//import java.util.ArrayList;
import android.view.inputmethod.InputMethodManager;


class Screen extends View {

    GRASP activity;

    float width;
    float height;

    Widget main_widget;
    
    public Screen(GRASP source) {
	super(source);

	activity = source;
	DisplayMetrics metrics =
	    source
	    .getResources()
	    .getDisplayMetrics();

	width = (float) metrics.widthPixels;
	height = (float) metrics.heightPixels;
	
	main_widget =
	    (new Below(new TextLine("text A"),
		       new TextLine("text B"),
		       new TextLine("text C"),
		       new TextLine("text D"),
		       new TextLine("text E"),
		       new TextLine("text F")))
	    .clippedTo(100, 200)
	    //.scrolledBy(40, 100)
	    //.displacedBy(20, 100)
	    ;
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

	main_widget.render(canvas, 0, 0, width, height);
	
	if (Utils.logger != null) {
	    Utils.logger.draw(canvas, 0, 0);
	}
    }
}
