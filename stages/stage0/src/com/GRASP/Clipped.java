package com.GRASP;

import android.graphics.Canvas;

class Clipped extends Widget {
    float vscroll = 0;
    float hscroll = 0;

    float _width;
    float _height;

    Widget target;
    
    public Clipped(float width, float height,
		   Widget target) {
	_width = width;
	_height = height;
	this.target = target;
    }
    
    @Override
    public void render(Canvas canvas,
		       float clip_left, float clip_top,
		       float clip_width, float clip_height) {
	canvas.save();
	canvas.translate(-hscroll, -vscroll);
	canvas.clipRect(hscroll, vscroll,
			hscroll+_width,
			vscroll+_height);
	target.render(canvas,
		      max(0, clip_left+hscroll),
		      max(0, clip_top+vscroll),
		      min(_width, clip_width),
		      min(_height, clip_height));
	canvas.restore();
    }

    @Override
    public float width() {
	return _width;
    }

    @Override
    public float height() {
	return _height;
    }

    public Clipped scrolledBy(float hor, float ver) {
	hscroll = max(0, min(target.width() - _width,
			     hscroll + hor));
	vscroll = max(0, min(target.height() - _height,
			     vscroll + ver));
		      
	return this;
    }

}
