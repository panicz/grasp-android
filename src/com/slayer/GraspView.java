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
    Transform transform;
    Point previous[] = { new Point(0,0), new Point(0,0) };
    Point current[] = { new Point(0,0), new Point(0,0) };
    
    public ArrayList<Asset> assets =
	new ArrayList<Asset>();
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
	transform = new Transform(l);
	log.log("GraspView created");

    }
    
    @Override
    protected void onDraw(Canvas canvas) {

	canvas.drawRGB(0, 0, 0);
	paint.setColor(Color.GREEN);
	    
	for (int i = 0;
	     i < assets.size();
	     i++) {
	    assets.get(i).draw(canvas,
			       paint,
			       transform);
	}

	if (editedShape != null) {
	    editedShape.draw(canvas,
			     paint,
			     transform);
	    editedShape.drawDeriv(canvas, paint,
				  40,
				  canvas.getHeight()-120,
				  40);
	    
	}

	paint.setColor(Color.WHITE);
	paint.setTypeface(font);
	paint.setTextSize(12);
	log.draw(canvas, 0, 20, paint);

    }

    String actionName(int action) {
	switch (action) {
	case MotionEvent.ACTION_DOWN:
	    return "DOWN";
	case MotionEvent.ACTION_POINTER_DOWN:
	    return "POINTER_DOWN";
	case MotionEvent.ACTION_MOVE:
	    return "MOVE";
	case MotionEvent.ACTION_UP:
	    return "UP";
	case MotionEvent.ACTION_POINTER_UP:
	    return "POINTER_UP";
	case MotionEvent.ACTION_OUTSIDE:
	    return "OUTSIDE";
	case MotionEvent.ACTION_CANCEL:
	    return "CANCEL";
	default:
	    return "UNKNOWN";
	}
    }

    boolean log_only = true;
    
    @Override
    public boolean
	onTouchEvent(MotionEvent event) {

	int action = event.getActionMasked();
        int pointerIndex = event.getActionIndex();
	int pointerCount = event.getPointerCount();
	/*
	if(action != MotionEvent.ACTION_MOVE) {
	    int id = event.getPointerId(pointerIndex);
	    log.log("ACTION_"+actionName(action)
		    +" id:"+id+", ("+
		    +event.getX(pointerIndex)+", "
		    +event.getY(pointerIndex)
		    +"), index="+pointerIndex);
	}
	
	if (log_only) {
	    invalidate();
	    return true;
	    }*/
	
	if (pointerCount == 1) {
	    switch (action) {
	    case MotionEvent.ACTION_DOWN:
	    case MotionEvent
		.ACTION_POINTER_DOWN: {
		    float x = event.getX();
		    float y = event.getY();
		    Point p = new Point(x, y);
		    previous[0].set(x, y);
		    if (editedShape == null) {
			editedShape =
			    new Shape(log);
			editedShape
			    .add(transform.unp(p));
			invalidate();
		    }
		    // jezeli to jedyny palec,
		    // zaczynamy rejestrowac
		    // ksztalt
		}
		break;

	    case MotionEvent.ACTION_UP:
	    case MotionEvent.ACTION_POINTER_UP:
	    case MotionEvent.ACTION_OUTSIDE:
	    case MotionEvent.ACTION_CANCEL:
		// jezeli rejestrowalismy
		// ksztalt, to teraz
		// pora na weryfikacje

		if (editedShape != null) {
		    editedShape.close();
		    log.log("");
		    if (editedShape.isBox()) {
			assets.add(new Box(editedShape));
		    }
		    else {
			assets.add(editedShape);
		    }
		    editedShape = null;

		    invalidate();
		}
		break;

	    case MotionEvent.ACTION_MOVE: {
		// jezeli jestesmy w trybie
		// rejestracji
		/*
		  log.log(event.getX(),
		  ", ",
		  event.getY());
		*/
		float x = event.getX();
		float y = event.getY();
		Point p = new Point(x, y);

		previous[0].set(x, y);

		if (editedShape != null) {
		    editedShape
			.add(transform.unp(p));
		    invalidate();
		}
	    }
		break;
	    }
	}
	else if(pointerCount == 2) {
	    editedShape = null;

	    if(pointerIndex >= 2) {
		return false;
	    }
	    
	    switch (action) {
	    case MotionEvent.ACTION_POINTER_DOWN:
		for (int i = 0; i < pointerCount; ++i) {
		    previous[i].set(event.getX(i),
				    event.getY(i));
		}
		/*
		log.log("pointer "+pointerIndex+" down, "
			+previous[0]+", "+previous[1]);
		*/
		invalidate();
	    return true;
	    case MotionEvent.ACTION_MOVE:
		for (int i = 0; i < pointerCount; ++i) {
		    current[i].set(event.getX(i),
				   event.getY(i));
		
		}

		Point adjusted [] = 
		transform
		    .adjusted(
			      previous[1],
			      current[1],
			      previous[0],
			      current[0]
			      );
		if (adjusted != null) {
		    for (int i = 0;
			 i < pointerCount;
			 ++i) {
			previous[i].set(current[i]);
		    }
		    invalidate();
		    return true;
		}
		else {
		    log.log("unadjusted!");
		    invalidate();
		}

	    case MotionEvent.ACTION_UP:
	    case MotionEvent.ACTION_POINTER_UP:
	    case MotionEvent.ACTION_OUTSIDE:
	    case MotionEvent.ACTION_CANCEL:
		transform.clear();
		
	    default:
		return false;
	    }

	}

	return true;
    }
	
}
