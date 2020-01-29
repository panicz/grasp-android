package com.slayer;


import java.io.Serializable;
import java.lang.Math;

class Vector implements Serializable {
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

}
