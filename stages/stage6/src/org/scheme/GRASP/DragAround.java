package org.scheme.GRASP;

import android.graphics.Canvas;
import java.lang.Math;

class DragAround implements Tile, Drag {

    public Bit target;
    public float x;
    public float y;
    
    public DragAround(Bit widget, float dx, float dy) {
	target = widget;
	assert(target.following_space() == null);
	x = dx;
	y = dy;
	/*
	if (widget instanceof Box) {
	    Box b = (Box) widget;
	    Line line = b.first_interline != null
		? b.first_interline.following_line
		: null;
	    Space space = line != null
		? line.first_space
		: null;
	    GRASP.log("dragging "+b.first_interline
		      +" "+line+" "+space);
	}
	*/
	
    }
    
    @Override
    public void move(Screen screen, float x, float y,
		     float dx, float dy) {
	
	this.x += dx;
	this.y += dy;
    }

    @Override
    public void drop(Screen screen, float x, float y,
		     float vx, float vy) {
	screen.overlay.removeLastOccurrence(this);
	if (Math.sqrt(vx*vx + vy*vy)
	    < Split.closing_threshold) {
	    screen.panel.insertAt(x, y, this, null);
	}
    }

    @Override
    public Drag inwards(Transform transform) {
	float x_ = transform.unx(x, y);
	float y_ = transform.uny(x, y);
	x = x_;
	y = y_;
	return this;
    }

    @Override
    public Drag outwards(Transform transform) {
	float x_ = transform.x(x, y);
	float y_ = transform.y(x, y);
	x = x_;
	y = y_;
	return this;
    }
    
    @Override
    public void render(Canvas canvas) {
	canvas.save();
	canvas.translate(x, y);
	target.render(canvas);
	canvas.restore();
    }

    @Override
    public float width() {
	return target.width();
    }

    @Override
    public float height() {
	return target.height();
    }

    @Override
    public void trySetSize(float x, float y) {
	target.trySetSize(x, y);
    }
}
