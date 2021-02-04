package com.GRASP;

import java.lang.Math;

class Vector {
    public float x;
    public float y;
    public Vector(Point from, Point to) {
	x = to.x - from.x;
	y = to.y - from.y;
    }
    public Vector(float _x, float _y) {
	x = _x;
	y = _y;
    }
    public float length() {
	return (float) Math.sqrt(x*x+y*y);
    }
    public float distanceTo(Vector p) {
	float dx = p.x - x;
	float dy = p.y - y;
	return (float) Math.sqrt(dx*dx+dy*dy);
    }

    public float distanceTo(float px, float py) {
	float dx = px - x;
	float dy = py - y;
	return (float) Math.sqrt(dx*dx+dy*dy);
    }

    public boolean near(Vector p) {
	return distanceTo(p) < 1;
    }
    
    public boolean near(float px, float py) {
	return distanceTo(px, py) < 1;
    }

    
    public Vector normalized() {
	float l = length();	
	return new Vector(x / l,
			  y / l);
    }
    public float dot(Vector v) {
	return x*v.x + y*v.y;
    }
    public float cross(Vector v) {
	return x*v.y - y*v.x;
    }
    public Vector set(float _x, float _y) {
	x = _x;
	y = _y;
	return this;
    }
    public Vector set(Vector v) {
	x = v.x;
	y = v.y;
	return this;
    }

    public Vector inc(float dx, float dy) {
	x += dx;
	y += dy;
	return this;
    }
    public String toString() {
	return "("+(int)x+", "+(int)y+")";
    }

}
