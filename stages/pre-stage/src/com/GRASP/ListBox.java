package com.GRASP;

//import android.graphics.Canvas;
//import java.util.ArrayList;
import java.lang.Math;
import android.graphics.Color;

//import android.graphics.RectF;

class ListBox extends MultiBox {

    @Override
    public void afterElementAdded(Box target, int index) {
    }
    
    public void addObject(Object x) {
	Box b;
	if (x instanceof Box) {
	    b = (Box) x;
	}
	else if(x instanceof String) {
	    b = new TextBox((String) x,
			    GRASP.default_font,
			    48,
			    Color.BLACK,
			    320,
			    60,
			    insensitive);
	}
	else if(x instanceof Button) {
	    b = new TextBox(((Button) x).text,
			    GRASP.default_font,
			    48,
			    Color.BLACK,
			    320,
			    60,
			    ((Button) x).action);
	}
	else {
	    GRASP.Log("attempted to put "
		      +"an unknown object "
		      +x+" to a ListBox");
	    return;
	}
	    
	MultiBox lag = new MultiBox(0,
				    area.height(),
				    b);
    
	addChild(lag, 0, 0);
	area.bottom += lag.getHeight();
	area.right = Math.max(area.right,
			      area.left
			      +lag.getWidth());
    }
    
    
    public ListBox(float l, float t,
		   Object... args) {
	super(l, t, l, t);
	
	for (Object x : args) {
	    addObject(x);
	}
    }
}
