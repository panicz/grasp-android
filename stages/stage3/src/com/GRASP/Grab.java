package com.GRASP;
import android.graphics.Canvas;

import java.lang.Math;

class Grab implements Transform {
    private float dx = 0.0f;
    private float dy = 0.0f;

    private float scale = 1.0f;
    private float unscale = 1.0f;
    
    private float rad = 0.0f;
    private float deg = 0.0f;
    private float sin = 0.0f;
    private float cos = 1.0f;

    public float getAngle() {
	return deg;
    }

    public void setAngle(float degrees) {
	deg = degrees;
	rad = (float) Math.toRadians(deg);
	sin = (float) Math.sin(rad);
	cos = (float) Math.cos(rad);
    }

    public float getScale() {
	return scale;
    }

    public void setScale(float scale) {
	assert(scale > 0);
	this.scale = scale;
	unscale = 1.0f/scale;
    }

    public float getLeft() {
	return dx;
    }

    public void setLeft(float x) {
	dx = x;
    }

    public float getTop() {
	return dy;
    }

    public void setTop(float y) {
	dy = y;
    }

       
    public void reset() {
	dx = 0.0f;
	dy = 0.0f;

	scale = 1.0f;
	unscale = 1.0f;
    
	rad = 0.0f;
	deg = 0.0f;
	sin = 0.0f;
	cos = 1.0f;
    }
    
    public Grab(float x, float y, float scale, float angle_deg) {
	assert(scale > 0);
	dx = x;
	dy = y;
	this.scale  = scale;
	unscale = 1.0f/scale;
	deg = angle_deg;
	rad = (float) Math.toRadians(deg);
	sin = (float) Math.sin(rad);
	cos = (float) Math.cos(rad);
    }

    public Grab() {
	this(0.0f, 0.0f, 1.0f, 0.0f);
    }

    
    Grab copy() {
	return new Grab(dx, dy, scale, deg);
    }
    
    void scrollBy(float ddx, float ddy) {
	float dx_ = x(dx, dy);
	float dy_ = y(dx, dy);

	float dxx = dx_+ddx;
	float dyy = dy_+ddy;

	dx_ = unx(dxx, dyy);
	dy_ = uny(dxx, dyy);

	dx = dx_;
	dy = dy_;
    }
    
    @Override
    public Transform canvas(Canvas canvas) {
	canvas.scale(scale, scale);
	canvas.rotate(deg);
	canvas.translate(dx, dy);
	return this;
    }

    @Override
    public float x(float ox, float oy) {
	return scale*(cos*(ox+dx) - sin*(oy+dy));
    }

    @Override
    public float y(float ox, float oy) {
	return scale*(sin*(ox+dx) + cos*(oy+dy));
    }

    @Override
    public Transform uncanvas(Canvas canvas) {
	canvas.translate(-dx, -dy);
	canvas.rotate(-deg);
	canvas.scale(unscale, unscale);

	return this;
    }
    
    @Override
    public float unx(float mx, float my) {
	return (cos*(mx) + sin*(my))*unscale - dx;
    }

    @Override
    public float uny(float mx, float my) {
	return (-sin*(mx) + cos*(my))*unscale - dy;
    }

    float [] pin_x = new float[2];
    float [] pin_y = new float[2];
    
    @Override
    public Transform anchor(float [] xs, float [] ys, byte n) {

	for (int i = 0; i < Math.min(n, 2); ++i) {
	    pin_x[i] = xs[i];
	    pin_y[i] = ys[i];
	}
	
	return this;
    }

    @Override
    public Transform towards(float [] xs, float [] ys, byte n) {
	if (n <= 0) {
	    return this;
	}
	if (n == 1) {
	    dx += (xs[0] - pin_x[0])*unscale;
	    dy += (ys[0] - pin_y[0])*unscale;
	    return this.anchor(xs, ys, n);
	}
	
	float px = pin_x[0]-pin_x[1];
	float py = pin_y[0]-pin_y[1];

	float d1 = (float) Math.sqrt(S.qr(px) + S.qr(py));

	float sx = xs[0]-xs[1];
	float sy = ys[0]-ys[1];
	
	float d2 = (float) Math.sqrt(S.qr(sx) + S.qr(sy));

	float scale_ = scale*d2/d1;
	float unscale_ = 1.0f/scale_;

	float da = (float)(Math.atan2(sy,sx) - Math.atan2(py,px));
	
	float rad_ = rad + da;
	float sin_ = (float) Math.sin(rad_);
	float cos_ = (float) Math.cos(rad_);

	dx -= (cos*pin_x[0] + sin*pin_y[0])*unscale
	    - (cos_*xs[0] + sin_*ys[0])*unscale_;
	dy -= (-sin*pin_x[0] + cos*pin_y[0])*unscale
	    - (-sin_*xs[0] + cos_*ys[0])*unscale_;

	rad = rad_;
	deg = (float) Math.toDegrees(rad_);
	sin = sin_;
	cos = cos_;
	scale = scale_;
	unscale = unscale_;
	
	return this.anchor(xs, ys, n);
    }

}
