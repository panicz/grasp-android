package com.slayer;


import java.io.Serializable;


class Point implements Serializable {

    public float x;
    public float y;
    public Point(float left, float top) {
	x = left;
	y = top;
    }
    public float distanceTo(Point p) {
	float dx = p.x - x;
	float dy = p.y - y;
	return (float) Math.sqrt(dx*dx+dy*dy);
    }

    public float distanceTo(float px, float py) {
	float dx = px - x;
	float dy = py - y;
	return (float) Math.sqrt(dx*dx+dy*dy);
    }

    public boolean near(Point p) {
	return distanceTo(p) < 1;
    }
    
    public boolean near(float px, float py) {
	return distanceTo(px, py) < 1;
    }
    
    public Point set(float _x, float _y) {
	x = _x;
	y = _y;
	return this;
    }

    public Point set(Point p) {
	x = p.x;
	y = p.y;
	return this;
    }
    
    public String toString() {
	return "("+(int)x+", "+(int)y+")";
    }

}
