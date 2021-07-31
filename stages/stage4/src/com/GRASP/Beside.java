package com.GRASP;

import android.graphics.Canvas;

class Beside extends Below {
    public Beside(Pad ... items) {
	contents = items;
    }

    @Override
    protected float horizontal(float accum, float value) {
	return super.vertical(accum, value);
    }

    @Override
    protected float vertical(float accum, float value) {
	return super.horizontal(accum, value);
    }

    @Override
    protected void translate(Canvas canvas, float w, float h) {
	canvas.translate(w, 0);
    }

    @Override
    protected float advance_height(float h) {
	return super.advance_width(h);
    }

    @Override
    protected float advance_width(float w) {
	return super.advance_height(w);
    }

    
}
