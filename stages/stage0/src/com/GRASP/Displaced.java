package com.GRASP;

import android.graphics.Canvas;

class Displaced extends Widget {
    float left;
    float top;

    Widget target;
    
    public Displaced(float left, float top, Widget target) {
	this.left = left;
	this.top = top;
	this.target = target;
    }
    
    @Override
    public void render(Canvas canvas,
		       float clip_left, float clip_top,
		       float clip_width, float clip_height) {
	canvas.save();
	canvas.translate(left, top);
	canvas.clipRect(0, 0, target.width(), target.height());
	target.render(canvas,
		      max(0, clip_left - left),
		      max(0, clip_top - top),
		      max(0, clip_width - left),
		      max(0, clip_height - top));
	canvas.restore();
    }
    //Skim skim(float x, float y);

    @Override
    public float width() {
	return left+target.width();
    }

    @Override
    public float height() {
	return top+target.height();
    }

}
