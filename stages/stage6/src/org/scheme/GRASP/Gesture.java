package org.scheme.GRASP;

import java.lang.Math;

abstract class Gesture {
    public String name;
    public abstract boolean recognizes(Shape shape,
				       Screen screen);
    public abstract void perform(Shape shape,
				 Screen screen);

    public static float angle(float deg) {
	if (!Float.isFinite(deg)) {
	    return deg;
	}
	while (deg <= -180) {
	    deg += 360;
	}
	while (deg > 180) {
	    deg -= 360;
	}
	return(deg);
    }
    
    public static boolean near(float a, float b,
			       float eps) {
	return Math.abs(a-b) <= eps;
    }
    
    public static float slope(Point p1, Point p2) {
	float dx = p2.x - p1.x;
	float dy = p2.y - p1.y;
	float r = (float) Math.hypot(dx, dy);
	float x = dx/r;
	float y = dy/r;
	float rad = (float) Math.atan2(y, x);
	assert(-Math.PI <= rad && rad <= Math.PI);
	return (float)(180.0f*rad/Math.PI);
    }

    public static float min(float ... vals) {
	float lo = Float.POSITIVE_INFINITY;
	for(int i = 0; i < vals.length; ++i) {
	    if(vals[i] < lo) {
		lo = vals[i];
	    }
	}
	return lo;
    }


    public static float max(float ... vals) {
	float hi = Float.NEGATIVE_INFINITY;
	for(int i = 0; i < vals.length; ++i) {
	    if(vals[i] > hi) {
		hi = vals[i];
	    }
	}
	return hi;
    }

    
}
