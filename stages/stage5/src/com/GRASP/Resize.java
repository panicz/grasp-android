package com.GRASP;

import android.graphics.Canvas;
//import java.lang.Math;

class Resize implements Drag {
    
    Bit target;
    float x0, y0;
    float w0, h0;

    
    Grab grab = new Grab();
    
    public Resize(Bit target, float x, float y) {
	this.target = target;
	x0 = x;
	y0 = y;
	w0 = target.width();
	h0 = target.height();
    }
    
    @Override
    public void move(Screen screen, float x, float y,
		     float dx, float dy) {
	float dw = grab.unx(x, y)-grab.unx(x0, y0);
	float dh  = grab.uny(x, y)-grab.uny(x0, y0);
	target.trySetSize(w0+dw, h0+dh);
    }

    @Override
    public void drop(Screen screen, float x, float y,
		     float vx, float vy) {
	if (Math.sqrt(vx*vx + vy*vy)
	    >= Split.closing_threshold) {
	    // pull the rug
	}
    }
    
    @Override
    public Drag outwards(Transform transform) {
	
	float x_ = transform.x(x0, y0);
	float y_ = transform.y(x0, y0);
	x0 = x_;
	y0 = y_;
	
	if (transform instanceof Grab) {
	    Grab g = (Grab) transform;
	    float a = g.getAngle();
	    float s = g.getScale();
	    float x = g.getLeft();
	    float y = g.getTop();

	    grab.setLeft(grab.x(x, y));
	    grab.setTop(grab.y(x, y));

	    grab.setAngle(grab.getAngle()+a);
	    grab.setScale(grab.getScale()*s);
	    
	}
	else if (transform instanceof Shift) {

	    Shift s = (Shift) transform;
	    float dx = grab.x(s.dx, s.dy);
	    float dy = grab.y(s.dx, s.dy);
	    
	    grab.setLeft(dx);
	    grab.setTop(dy); 
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
