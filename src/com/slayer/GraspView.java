package com.slayer;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.Typeface;
import android.view.MotionEvent;
import android.view.View;
import java.util.ArrayList;


class GraspView extends View {
    Paint paint;
    Typeface font;
    Rect bounds = new Rect();
	
    public ArrayList<Shape> shapes =
	new ArrayList<Shape>();
    public Logger log;

    public Shape editedShape = null;
	
    public GraspView(Context context, Logger l) {
	super(context);
	paint = new Paint();
	log = l;
	font = Typeface
	    .createFromAsset(context
			     .getAssets(),
			     "font.ttf");
    }
    
    @Override
    protected void onDraw(Canvas canvas) {

	canvas.drawRGB(0, 0, 0);
	paint.setColor(Color.GREEN);
	paint.setTypeface(font);
	paint.setTextSize(9);
	log.draw(canvas, 0, 0, paint);
	    
	for (int i = 0;
	     i < shapes.size();
	     i++) {
	    shapes.get(i).draw(canvas,
			       paint);
	}

	if (editedShape != null) {
	    editedShape.draw(canvas,
			     paint);
	}

    }
	
    @Override
    public boolean
	onTouchEvent(MotionEvent event) {

	int action =
	    (event.getAction()
	     & MotionEvent.ACTION_MASK);
	int pointerIndex =
	    (event.getAction()
	     & (MotionEvent
		.ACTION_POINTER_ID_MASK))
	    >> (MotionEvent
		.ACTION_POINTER_ID_SHIFT);
	int pointerCount =
	    event.getPointerCount();
	    
	    
	if (pointerCount == 1
	    && pointerIndex == 0) {
	    switch (action) {
	    case MotionEvent.ACTION_DOWN:
	    case MotionEvent
		.ACTION_POINTER_DOWN:

		if (editedShape == null) {
		    editedShape =
			new Shape(log);
		    editedShape
			.add(event.getX(),
			     event.getY());
			
		}
		// jezeli to jedyny palec,
		// zaczynamy rejestrowac
		// ksztalt
		break;

	    case MotionEvent.ACTION_UP:
	    case MotionEvent
		.ACTION_POINTER_UP:
	    case MotionEvent.ACTION_OUTSIDE:
	    case MotionEvent.ACTION_CANCEL:
		// jezeli rejestrowalismy
		// ksztalt, to teraz
		// pora na weryfikacje

		if (editedShape != null) {
		    shapes.add(editedShape);
		    editedShape = null;

		    //invalidate();
		}
		break;

	    case MotionEvent.ACTION_MOVE:
		// jezeli jestesmy w trybie
		// rejestracji
		/*
		log.log(event.getX(),
			", ",
			event.getY());
		*/

		if (editedShape != null) {
		    editedShape
			.add(event.getX(),
			     event.getY());
		    //invalidate();
		}
		    
		break;
	    }
	}
	else if(pointerCount == 2
		|| pointerIndex == 1){
	    editedShape = null;
	}

	invalidate();
	return true;
    }
	
}
