package com.slayer;


import java.io.Serializable;
import java.lang.Math;
import java.util.ArrayList;
import android.graphics.Canvas;

import android.graphics.Paint;


class Shape implements Serializable {
    //static final long serialVersionUID = 0x54a9e;
    public ArrayList<Point> points =
	new ArrayList<Point>();

    Logger log;
    
    float left = Float.POSITIVE_INFINITY;
    float right = Float.NEGATIVE_INFINITY;
    float top = Float.POSITIVE_INFINITY;
    float bottom = Float.NEGATIVE_INFINITY;

    public Shape(Logger l) {
	log = l;
    }
    
    final public void draw(Canvas canvas,
			   Paint paint) {
	float h = paint.getTextSize()+1;
	for(int i = 1;
	    i < points.size();
	    i++) {
	    Point p1 = points.get(i-1);
	    Point p2 = points.get(i);
	    canvas.drawLine(p1.x,
			    p1.y,
			    p2.x,
			    p2.y,
			    paint);
	    canvas.drawCircle(p2.x,
			      p2.y,
			      5,
			      paint);
	    String/*Builder*/ s =
		//new StringBuilder();
		"("+(int)p2.x+", "
		+(int)p2.y+")";
	    //s.append("(");
	    //s.append(p2.x);
	    //s.append
	    canvas.drawText(s,
			    p2.x + h,
			    p2.y + h,
			    paint);
	}
    }
    static float distance(Point p1,
			  Point p2) {
	return (float) Math
	    .sqrt(Math.pow(p1.x-p2.x, 2)
		  +Math.pow(p1.y-p2.y, 2));
    }
    
    public void add(float x, float y) {
	Point p1 = new Point(x, y);
	int l = points.size();

	if (l < 2) {
	    points.add(p1);
	}
	else {
	    Point p2 = points.get(l-1);
	    Point p3 = points.get(l-2);
	    float d = deriv(p1, p2, p3);
	    
	    if (Math.abs(d) <= 0.2
		|| distance(p1, p2) <= 20) {
		//log.log("merge");
		p2.x = x;
		p2.y = y;
	    }
	    else {
		points.add(p1);
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
	/*
	log.log("("+
		(int) p3.x+", "+(int)p3.y
		+") -/"+
		(int) v2.x+", "+(int)v2.y
		+"/"+v2.length()+"/"+
		n2.x+", "+n2.y
		+"/-> ("+
		(int) p2.x+", "+(int)p2.y
		+") -/"+
		(int) v1.x+", "+(int)v1.y
		+"/"+v1.length()+"/"+
		n1.x+", "+n1.y
		+"/-> ("+
		(int) p1.x+", "+(int)p1.y
		+") : "+s+", "+c+", "+d);
	*/
	return n1.cross(n2);
    }
	
    public Float lastDeriv() {
	int l = points.size();
	if (l < 3) {
	    return null;
	}
	Point p1 = points.get(l-1);
	Point p2 = points.get(l-2);
	Point p3 = points.get(l-3);
	return deriv(p1, p2, p3);
    }
}
