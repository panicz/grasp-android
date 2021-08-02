package com.GRASP;

import android.graphics.Canvas;

class Beside extends Below {
    public Beside(Pad ... items) {
	contents = items;
    }

    @Override
    Pad itemAt(float x, float y) {
	return super.itemAt(x, y);
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
	return 0;
    }

    @Override
    protected float advance_width(float w) {
	return w;
    }

    @Override
    public void trySetSize(float x, float y) {
    }
    
}
