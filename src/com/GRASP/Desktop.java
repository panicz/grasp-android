package com.GRASP;

import android.content.Context;
import android.graphics.Canvas;
//import android.graphics.RectF;
import android.util.DisplayMetrics;
//import android.view.MotionEvent;
import android.view.View;
//import java.util.ArrayList;
import android.view.inputmethod.InputMethodManager;


class Desktop extends View {

    public Stage stage;
    Context context;
    public Desktop(Context context) {
	super(context);

	this.context = context;
	DisplayMetrics metrics =
	    context
	    .getResources()
	    .getDisplayMetrics();
	stage =
	    new Stage(this,
		      0, 0,
		      (float) metrics.widthPixels,
		      (float) metrics.heightPixels);	
    }

    public void showKeyboard() {
	if (requestFocus()) {
	    InputMethodManager imm = (InputMethodManager)
                context
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
	stage.draw(canvas);
	if (GRASP.log != null) {
	    GRASP.log.draw(canvas, 0, 0);
	}
    }
}
