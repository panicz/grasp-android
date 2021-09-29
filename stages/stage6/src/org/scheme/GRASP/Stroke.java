package org.scheme.GRASP;

import android.graphics.RectF;

//import java.io.Serializable;
import java.lang.Math;
import java.util.ArrayList;
import java.util.List;

//import java.util.Comparator;
//import java.util.Collections;

import android.graphics.Color;
import android.graphics.Canvas;
import android.graphics.Paint;


class Stroke {
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
    
    public RectF rect =
	new RectF(Float.POSITIVE_INFINITY,
		  Float.POSITIVE_INFINITY,
		  Float.NEGATIVE_INFINITY,
		  Float.NEGATIVE_INFINITY);

    public Stroke() {
	paint.setColor(Color.LTGRAY);
	paint.setStrokeWidth(4);
    }

    public float getWidth() {
	return rect.right - rect.left;
    }

    public float getHeight() {
	return rect.bottom - rect.top;
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
	//float h = paint.getTextSize()+1;
	for(int i = 1; i < points.size(); i++) {
	    Point p1 = points.get(i-1).point;
	    Point p2 = points.get(i).point;
	    canvas.drawLine(p1.x, p1.y, p2.x, p2.y,paint);
	    //canvas.drawCircle(p2.x, p2.y, 5, paint);
	}
    }

    public Stroke toOrigin() {
	for (DerivPoint dp : points) {
	    dp.point.x -= rect.left;
	    dp.point.y -= rect.top;
	}
	rect.right -= rect.left;
	rect.bottom -= rect.top;
	
	rect.left = 0;
	rect.top = 0;
	return this;
    }

    public void add(float x, float y) {
	add(new Point(x, y));
    }
    
    public void add(Point p1) {
	float x = p1.x;
	float y = p1.y;
	int l = points.size();

	if (l < 2) {
	    points.add(new DerivPoint(p1, l));
	}
	else {
	    Point p2 = points.get(l-1).point;
	    Point p3 = points.get(l-2).point;
	    float d = deriv(p1, p2, p3);
	    
	    if (false && (Math.abs(d) <= 0.15
			  || p1.distanceTo(p2) <= 7)) {
		p2.x = x;
		p2.y = y;
	    }
	    else {
		points.add(new DerivPoint(p1, d, l));
		derivative.add(d);
	    }
	}
	
	if (x < rect.left) {
	    rect.left = x;
	}
	if (x > rect.right) {
	    rect.right = x;
	}
	if (y < rect.top) {
	    rect.top = y;
	}
	if (y > rect.bottom) {
	    rect.bottom = y;
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
		       Point p3) {
	Vector v1 = new Vector(p1, p2);
	Vector v2 = new Vector(p2, p3);
	Vector n1 = v1.normalized();
	Vector n2 = v2.normalized();
	//float s = n1.cross(n2);
	//float c = n1.dot(n2);
	//float d = (float) Math.atan2(s, c);
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

    // adapted from
    // https://rosettacode.org/wiki/Ramer-Douglas-Peucker_line_simplification#Java
    static float perpendicularDistance(Point pt, Point lineStart, Point lineEnd) {
        float dx = lineEnd.x - lineStart.x;
        float dy = lineEnd.y - lineStart.y;
 
        // Normalize
        float mag = (float) Math.hypot(dx, dy);
        if (mag > 0.0f) {
            dx /= mag;
            dy /= mag;
        }
        float pvx = pt.x - lineStart.x;
        float pvy = pt.y - lineStart.y;
 
        // Get dot product (project pv onto normalized direction)
        float pvdot = dx * pvx + dy * pvy;
 
        // Scale line direction vector and subtract it from pv
        float ax = pvx - pvdot * dx;
        float ay = pvy - pvdot * dy;
 
        return (float) Math.hypot(ax, ay);
    }
 
    static List<Point> RDP(List<Point> input, float epsilon) {
	// Ramer-Douglas-Peucker
	if (input.size() <= 2) {
	    return input;
	}

	List<Point> out = new ArrayList<Point>();
	
	float dmax = 0.0f;
        int index = 0;
        int end = input.size() - 1;
        for (int i = 1; i < end; ++i) {
            float d = perpendicularDistance(input.get(i),
					    input.get(0),
					    input.get(end));
            if (d > dmax) {
                index = i;
                dmax = d;
            }
        }
 
        // If max distance is greater than epsilon, recursively simplify
        if (dmax > epsilon) {
            List<Point> firstLine = input.subList(0, index + 1);
            List<Point> lastLine = input.subList(index, input.size());

            List<Point> recResults1 = RDP(firstLine, epsilon);
            List<Point> recResults2 = RDP(lastLine, epsilon);
 
            // build the result list
            out.addAll(recResults1.subList(0, recResults1.size() - 1));
            out.addAll(recResults2);

        } else {
            out.clear();
            out.add(input.get(0));
            out.add(input.get(input.size() - 1));
        }

	return out;
    }
    
    public Stroke simplify(float epsilon) {
	Stroke result = new Stroke();
	List<Point> input = new ArrayList<Point>();
	for (DerivPoint dp : points) {
	    input.add(dp.point);
	}
	List<Point> simplified = RDP(input, epsilon);
	for (Point p : simplified) {
	    result.add(p);
	}
	return result;
    }
}
