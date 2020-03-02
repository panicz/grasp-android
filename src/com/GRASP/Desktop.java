package com.GRASP;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.RectF;
import android.util.DisplayMetrics;
import android.view.MotionEvent;
import android.view.View;
import java.util.ArrayList;


class Desktop extends View {

    public Box stage;
    public Desktop(Context context) {
	super(context);

	DisplayMetrics metrics =
	    context.getResources().getDisplayMetrics();
	stage =
	    new Stage(this,
		      0, 0,
		      (float) metrics.widthPixels,
		      (float) metrics.heightPixels);
	
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
