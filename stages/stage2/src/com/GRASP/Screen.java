package com.GRASP;

import android.content.Context;
import android.graphics.Canvas;
//import android.graphics.RectF;
import android.util.DisplayMetrics;
import android.view.MotionEvent;
import android.view.View;
import java.util.ArrayList;
import android.view.inputmethod.InputMethodManager;


class Screen extends View {

    Interactions view;

    GRASP activity;
    
    public float width;
    public float height;

    ArrayList<Shape> shapes = new ArrayList<Shape>();
    
    Shape shape = null;

    boolean[] finger = new boolean[] {
	false, false, false, false, false,
	false, false, false, false, false
    };
    
    float[] x = new float[10];
    float[] y = new float[10];
    
    public Screen(GRASP source) {
	super(source);

	activity = source;
	DisplayMetrics metrics =
	    source
	    .getResources()
	    .getDisplayMetrics();

	width = (float) metrics.widthPixels;
	height = (float) metrics.heightPixels;

	view = new Editor(0, 0, width, height, null, 0, 0);
    }

   
    Split [] split = new Split[] {
	null, null, null, null, null,
	null, null, null, null, null
    };
    
    public boolean onDown(MotionEvent event) {
	int i = event.getActionIndex();
	int p = event.getPointerId(i);
	int n = event.getPointerCount();
	assert(!finger[p]);
	finger[p] = true;

	x[p] = event.getX(i);
	y[p] = event.getY(i);

	if (p > 0 && shape != null) {
	    shape = null;
	    shapes.clear();
	}

	if ((split[p] = view.splitUnder(x[p], y[p])) != null) {
	    split[p].startResizing(x[p], y[p]);
	}
	else if (n == 1 && p == 0 && shape == null) {
	    shape = new Shape();
	}
	
	
	/* co aie dzieje podczas wcisniecia? */
	// jezeli jesteśmy w trybie EnteringShape i dotknelismy
	// pierwszym palcem palku dzielacej ekran,
	// to przechodzimy w tryb ResizingViews z odpowiednio
	// ustawionym targetem

	// jezeli jestesmy w trybie EnteringShape i dotknelismy
	// pierwszym palcem na nieprzypietym obiekcie
	// to wchodzimynw tryb MovingBoxes i przenosimy
	// ten obiekt do warstwy przesuwanych obiektow
	
	// jezeli jestesmy w trybie EnteringShape i
	// dotknelismy pierwszym palcem na nieprzypietym
	// dokumencie, to zaczynamy rysowac ksztalt

	// jezeli jestesmy w trybie MovingBoxes,

	return true;

    }

    public boolean onMotion(MotionEvent event) {
	/* co sie dzieje podczas ruchu? */

	int n = event.getPointerCount();
	int max_finger = -1;
	    
	for (int i = 0; i < n; ++i) {
	    int p = event.getPointerId(i);
	    x[p] = event.getX(i);
	    y[p] = event.getY(i);
	    assert(finger[p]);
	    max_finger = (p > max_finger) ? p : max_finger;

	    if (split[p] != null) {
		split[p].resizeTo(x[p], y[p]);
		invalidate();
	    }
	}
	
	if (max_finger == 0
	    && n == 1
	    && shape != null) {
	    shape.add(x[0], y[0]);
	    GRASP._log.update("("+x[0]+", "+y[0]+")");
	    invalidate();
	}
	return true;
    }

    public boolean onUp(MotionEvent event,float vx, float vy) {
	/* co sie dzieje przy puszczeniu? */
	int i = event.getActionIndex();
	int p = event.getPointerId(i);
	assert(finger[p]);
	finger[p] = false;

	if (split[p] != null) {
	    
	    view = view.finishResizing(split[p], vx, vy);
	    
	    return true;
	}
	
	if (shape != null && p == 0) {
	    
	    if (shapes.isEmpty()) {
		/*
		 * a horizontal line makes a VerticalSplit,
		 * while a vertical line makes a 
		 * HorizontalSplit
		 */
		if (shape.isHorizontalLine()
		    && view
		    .canBeSplittedVerticallyBy(shape.rect)) {
		    // powinniśmy wziąć wszystkie
		    // dokumenty/widoki, przez które
		    // przechodzi nasza kreska,
		    // i podzielić je względem linii
		    // podzia
		    view=view.splitVerticallyBy(shape.rect);
		    //GRASP.log(view.toString());
		    shape = null;
		    return true;
		}
		else if(shape.isVerticalLine()
			&& view
			.canBeSplittedHorizontallyBy(shape
						     .rect)) {
		    view=view.splitHorizontallyBy(shape.rect);
		    //GRASP.log(view.toString());
		    shape = null;
		    return true;
		}
	    }
	    shapes.add(shape);
	    shape = null;
	}
	return true;
    }
    
    public boolean onDoubleTap(MotionEvent e) {
	/* co sie dzieje przy podwojnym kliknieciu? */
	
        return true;
    }

    public boolean onSingleTap(MotionEvent e) {
	/* co sie dzieje przy kliknieciu? */
	return true;
    }

    public boolean onLongPress(MotionEvent event) {
	/* co sie dzieje przy przytrzymaniu?*/
	GRASP._log.clear();
	return true;
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
	GRASP._log.draw(canvas, 0, 0);
	
	view.render(canvas);

	for (Shape shape : shapes) {
	    shape.draw(canvas);
	}
	
	if (shape != null) {
	    shape.draw(canvas);
	}
    }
    
}
