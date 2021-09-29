package org.scheme.GRASP;
import android.os.SystemClock;

class Point {
    public static long start_ns =
	SystemClock.elapsedRealtimeNanos();
    
    public float x;
    public float y;
    public float z;
    public Point(float left, float top, float depth) {
	x = left;
	y = top;
	z = depth;
    }

    public Point(float left, float top) {
	x = left;
	y = top;
	z = (SystemClock.elapsedRealtimeNanos()
	     - start_ns)*0.000000001f;
    }

    public Point() {
	x = y = z = Float.NaN;
    }
    
    public float distanceTo(Point p) {
	float dx = p.x - x;
	float dy = p.y - y;
	return (float) Math.hypot(dx, dy);
    }

    public float distanceTo(float px, float py) {
	float dx = px - x;
	float dy = py - y;
	return (float) Math.hypot(dx,dy);
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

    public Point transform(Transform t) {
	set(t.x(x, y), t.y(x, y));
	return this;
    }
    
    public Point untransform(Transform t) {
	set(t.unx(x, y), t.uny(x, y));
	return this;
    }
    
    public String toString() {
	return "("+(int)x+", "+(int)y+")";
    }
}
