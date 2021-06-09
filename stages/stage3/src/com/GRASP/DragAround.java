package com.GRASP;

import android.graphics.Canvas;
import java.lang.Math;

class DragAround implements Widget, Drag {

    public Bit target;
    public float x;
    public float y;
    
    public DragAround(Bit widget, float dx, float dy) {
	target = widget;
	assert(target.following_space() == null);
	x = dx;
	y = dy;
	GRASP.last_known_edit_instance.overlay.push(this);
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
	if (Math.sqrt(vx*vx + vy*vy) < Split.closing_threshold) {
	    screen.panel.insertAt(x, y, this);
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

}
