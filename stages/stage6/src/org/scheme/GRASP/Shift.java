package org.scheme.GRASP;
import android.graphics.Canvas;

class Shift implements Transform {
    public float dx = 0;
    public float dy = 0;
    
    Shift set(float x, float y) {
	dx = x;
	dy = y;
	return this;
    }

    @Override
    public Transform canvas(Canvas target) {
	return this;
    }

    @Override
    public float x(float ox, float oy) {
	return ox+dx;
    }

    @Override
    public float y(float ox, float oy) {
	return oy+dy;
    }

    @Override
    public Transform uncanvas(Canvas target) {
	return this;
    }

    @Override
    public float unx(float mx, float my) {
	return mx-dx;
    }

    @Override
    public float uny(float mx, float my) {
	return my-dy;
    }

    float ax, ay;
    
    @Override
    public Transform anchor(float [] xs, float [] ys, byte n) {
	if(n >= 1) {
	    ax = xs[0];
	    ay = ys[0];
	}
	return this;
    }

    @Override
    public Transform towards(float [] xs, float [] ys, byte n) {
	if (n >= 1) {
	    dx += xs[0] - ax;
	    dy += ys[0] - ay;
	}
	return anchor(xs, ys, n);
    }

}
