package com.GRASP;

import android.graphics.Canvas;
//import java.lang.Math;

class Resize implements Drag {
    
    Bit target;

    Grab grab = new Grab();
    
    public Resize(Bit target) {
	this.target = target;
    }
    
    @Override
    public void move(Screen screen, float x, float y,
		     float dx, float dy) {
	float w = grab.unx(x, y);
	float h = grab.uny(x, y);
	target.trySetSize(w, h);
    }

    @Override
    public void drop(Screen screen, float x, float y,
		     float vx, float vy) {
    }
    
    @Override
    public Drag outwards(Transform transform) {
	if (transform instanceof Grab) {
	    grab.setAngle(((Grab)transform).getAngle());
	    grab.setScale(((Grab)transform).getScale());
	    grab.setLeft(grab.getLeft()+((Grab)transform).getLeft());
	    grab.setTop(grab.getTop()+((Grab)transform).getTop());
	}
	else if (transform instanceof Shift) {
	    grab.setLeft(grab.getLeft()
			 + ((Shift)transform).dx);
	    grab.setTop(grab.getTop()
			+ ((Shift)transform).dy);
	}
	else {
	    GRASP.log("ignoring unsupported transform "
		      +transform);
	}
	return this;
    }

    @Override
    public Drag inwards(Transform transform) {
	GRASP.log("ignoring inwards transform "
		      +transform);
	return this;
    }

}
