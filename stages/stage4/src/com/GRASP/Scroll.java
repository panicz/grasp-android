package com.GRASP;

import android.graphics.Canvas;
import java.lang.Math;

class Scroll implements Pad, Drag {

    float _width;
    float _height;
    float hscroll = 0;
    float vscroll = 0;

    Pad target;

    public Scroll(Pad target, float w, float h) {
	this.target = target;
	_width = w;
	_height = h;
    }
    
    @Override
    public void render(Canvas canvas) {
	canvas.save();
	canvas.clipRect(0, 0, _width, _height);
	canvas.translate(-hscroll, -vscroll);
	target.render(canvas);
	canvas.restore();
    }

    @Override
    public void move(Screen screen, float x, float y,
		     float dx, float dy) {
	hscroll =
	    Math.max(0.0f, Math.min(hscroll - dx,
				    target.width() - _width));
	vscroll =
	    Math.max(0.0f, Math.min(vscroll - dy,
				    target.height() - _height));

	// mozemy tu jeszcze dac jakis margines, zeby w momencie
	// puszczania moglo (po przekroczeniu go) odsprezynowac
    }

    @Override
    public void drop(Screen screen, float x, float y,
		     float vx, float vy) {
	// mozemy tutaj dodac animacje sprezynowania
    }
    
    @Override
    public float width() {
	return _width;
    }

    @Override
    public float height() {
	return _height;
    }

    @Override
    public void trySetSize(float x, float y) {
	_width = x;
	_height = y;
    }
    
    @Override
    public Drag onPress(Screen screen, byte finger,
			float x, float y) {
	return this;
    }

    @Override
    public void onClick(Screen screen, byte finger,
			float x, float y) {
	target.onClick(screen, finger,
		       x + hscroll, y + vscroll);
    }

    @Override
    public Drag onSecondPress(Screen screen, byte finger,
			      float x, float y) {
	return target.onSecondPress(screen, finger,
				    x + hscroll,
				    y + vscroll);
    }

    @Override
    public void onDoubleClick(Screen screen, byte finger,
			      float x, float y) {
	target.onDoubleClick(screen, finger,
			     x + hscroll,
			     y + vscroll);
    }

    @Override
    public Drag onHold(Screen screen, byte finger,
		       float x, float y) {
	return target.onHold(screen, finger,
			     x + hscroll,
			     y + vscroll);
    }


    @Override
    public void onDragOver(Screen screen, byte finger,
			   float x, float y) {
	target.onDragOver(screen, finger,
			  x + hscroll,
			  y + vscroll);
    }

    @Override
    public void onDragOut(Screen screen, byte finger) {
	target.onDragOut(screen, finger);
    }

    @Override
    public void onRelease(Screen screen, byte finger,
			  float x, float y) {
	target.onRelease(screen, finger,
			 x + hscroll,
			 y + vscroll);
    }

    @Override
    public Drag outwards(Transform transform) {
	return this;
    }

    @Override
    public Drag inwards(Transform transform) {
	return this;
    }

}
