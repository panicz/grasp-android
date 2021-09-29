package org.scheme.GRASP;

import android.graphics.Canvas;
//import java.lang.Math;

class Resize implements Drag {
    
    Bit target;
    float x0, y0;
    float w0, h0;

    Box parent;
    Space preceding_space;
    Line line;
    float xp, yp;
    
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
	if (target != null) {
	    float dw = grab.unx(x, y)-grab.unx(x0, y0);
	    float dh  = grab.uny(x, y)-grab.uny(x0, y0);
	    /*
	      GRASP.log("("+(int)w0+", "+(int)h0+") -> ("
	      +(int)(w0+dw)+", "+(int)(h0+dh)+")"
	      +"/@("+(int)dw+", "+(int)dh+")");
	    */
	    target.trySetSize(w0+dw, h0+dh);
	}
    }

    static DragAround throwAround =
	new DragAround(null, 0, 0);
    
    void pull_the_rug() {
	preceding_space.remove_following_bit(line);
	Box rug = (Box) target;
	target = null;
	float accumulated_height = 0;
	Interline interline;
	Line line = null;
	
	for (interline = rug.first_interline;
	     interline != null;
	     interline = interline.following_line
		 .next_interline) {
	    
	    accumulated_height += interline.height;

	    line = interline.following_line;

	    if(line == null) {
		break;
	    }

	    if (line.first_space == null) {
		break;
	    }
	    
	    float line_height = line.height();

	    while (line.first_space
		   .following_bit != null) {
		throwAround.x = xp + Box.parenWidth
		    + line.first_space.width;
		
		throwAround.y = yp + accumulated_height;
		
		Bit bit = line.first_space
		    .remove_following_bit(line);
	       
		if (bit == null) {
		    break;
		}

		throwAround.target = bit;
		
		parent.insertAt(throwAround.x,
				throwAround.y,
				throwAround,
				null);
		
	    }
	    accumulated_height += line_height;
	}

    }

    
    @Override
    public void drop(Screen screen, float x, float y,
		     float vx, float vy) {
	if (Math.sqrt(vx*vx + vy*vy)
	    >= Split.closing_threshold) {
	    target.trySetSize(w0, h0);
	    if(target instanceof Box) {
		pull_the_rug();
	    }
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
