package com.GRASP;


import java.io.Serializable;
import java.lang.Math;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Collections;

import android.graphics.Color;

import android.graphics.Canvas;

import android.graphics.Paint;


class Shape extends IdleBox {
    //static final long serialVersionUID = 0x54a9e;
    class DerivPoint {
	public Float deriv;
	public Point point;
	public int index = -1;
	public DerivPoint(Point p, float d, int i) {
	    point = p;
	    deriv = d;
	    index = i;
	}
	public DerivPoint(Point p, int i) {
	    point = p;
	    deriv = Float.NaN;
	    index = i;
	}   
    };
    
    public ArrayList<DerivPoint> points =
	new ArrayList<DerivPoint>();

    public ArrayList<Float> derivative =
	new ArrayList<Float>();

    Paint paint = new Paint();
    
    public float left = Float.POSITIVE_INFINITY;
    public float right = Float.NEGATIVE_INFINITY;
    public float top = Float.POSITIVE_INFINITY;
    public float bottom = Float.NEGATIVE_INFINITY;


    public Shape() {
	paint.setColor(Color.BLACK);
	paint.setStrokeWidth(4);
    }
    
    final public void drawDeriv(Canvas canvas,
				Paint paint,
				float x, float y,
				float h) {
	for(int i = 0; i < derivative.size(); ++i) {
	    canvas.drawLine(x+i, y,
			    x+i, y + derivative.get(i)*h,
			    paint);
	}
    }
    
    final public void draw(Canvas canvas) {
	float h = paint.getTextSize()+1;
	for(int i = 1; i < points.size(); i++) {
	    Point p1 = points.get(i-1).point;
	    Point p2 = points.get(i).point;
	    canvas.drawLine(p1.x, p1.y, p2.x, p2.y,paint);
	    //canvas.drawCircle(p2.x, p2.y, 5, paint);
	}
    }
    
    public void add(Point p) {
	add(p.x, p.y);
    }
    
    public void add(float x, float y) {
	Point p1 = new Point(x, y);
	int l = points.size();

	if (l < 2) {
	    points.add(new DerivPoint(p1, l));
	}
	else {
	    Point p2 = points.get(l-1).point;
	    Point p3 = points.get(l-2).point;
	    float d = deriv(p1, p2, p3);
	    
	    if (false && Math.abs(d) <= 0.15
		|| p1.distanceTo(p2) <= 7) {
		p2.x = x;
		p2.y = y;
	    }
	    else {
		points.add(new DerivPoint(p1, d, l));
		derivative.add(d);
	    }
	}
	
	if (x < left) {
	    left = x;
	}
	if (x > right) {
	    right = x;
	}
	if (y < top) {
	    top = y;
	}
	if (y > bottom) {
	    bottom = y;
	}
    }

    public void close() {
	int l = points.size();
	if (l > 1) {
	    DerivPoint pl2 = points.get(l-2);
	    DerivPoint pl1 = points.get(l-1);

	    DerivPoint p0 = points.get(0);
	    DerivPoint p1 = points.get(1);

	    p0.deriv = deriv(p0.point,
			     pl1.point,
			     pl2.point);
	
	    p1.deriv = deriv(p1.point,
			     p0.point,
			     pl1.point);
	
	}
    }

    public float deriv(Point p1,
		       Point p2,
		       Point p3)
    {
	    
	Vector v1 = new Vector(p1, p2);
	Vector v2 = new Vector(p2, p3);
	Vector n1 = v1.normalized();
	Vector n2 = v2.normalized();
	float s = n1.cross(n2);
	float c = n1.dot(n2);
	float d = (float) Math.atan2(s, c);
	return n1.cross(n2);
    }
	
    public Float lastDeriv() {
	int l = points.size();
	if (l < 3) {
	    return null;
	}
	Point p1 = points.get(l-1).point;
	Point p2 = points.get(l-2).point;
	Point p3 = points.get(l-3).point;
	return deriv(p1, p2, p3);
    }
}
