package com.slayer;


import java.io.Serializable;
import java.lang.Math;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Collections;


import android.graphics.Canvas;

import android.graphics.Paint;


class Shape implements Asset {
    //static final long serialVersionUID = 0x54a9e;
    class DerivPoint {
	public Float deriv;
	public Point point;
	public DerivPoint(Point p, float d) {
	    point = p;
	    deriv = d;
	}
	public DerivPoint(Point p) {
	    point = p;
	    deriv = Float.NaN;
	}   
    };
    
    public ArrayList<DerivPoint> points =
	new ArrayList<DerivPoint>();

    public ArrayList<Float> derivative =
	new ArrayList<Float>();
    
    Logger log;
    
    public float left = Float.POSITIVE_INFINITY;
    public float right = Float.NEGATIVE_INFINITY;
    public float top = Float.POSITIVE_INFINITY;
    public float bottom = Float.NEGATIVE_INFINITY;

    public Shape(Logger l) {
	log = l;
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
    
    final public void draw(Canvas canvas,
			   Paint paint,
			   Transform transform) {
	float h = paint.getTextSize()+1;
	for(int i = 1; i < points.size(); i++) {
	    Point p1 = transform.p(points.get(i-1).point);
	    Point p2 = transform.p(points.get(i).point);
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
	    /*
	    canvas.drawText(s,
			    p2.x + h,
			    p2.y + h,
			    paint);*/
	}
    }
    static float distance(Point p1,
			  Point p2) {
	return (float) Math
	    .sqrt(Math.pow(p1.x-p2.x, 2)
		  +Math.pow(p1.y-p2.y, 2));
    }

    public void add(Point p) {
	add(p.x, p.y);
    }
    
    public void add(float x, float y) {
	Point p1 = new Point(x, y);
	int l = points.size();

	if (l < 2) {
	    points.add(new DerivPoint(p1));
	}
	else {
	    Point p2 = points.get(l-1).point;
	    Point p3 = points.get(l-2).point;
	    float d = deriv(p1, p2, p3);
	    
	    if (false && Math.abs(d) <= 0.15
		|| distance(p1, p2) <= 7) {
		//log.log("merge");
		p2.x = x;
		p2.y = y;
	    }
	    else {
		points.add(new DerivPoint(p1, d));
		derivative.add(d);
		log.log(d);
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
	Point p1 = points.get(l-1).point;
	Point p2 = points.get(l-2).point;
	Point p3 = points.get(l-3).point;
	return deriv(p1, p2, p3);
    }

    public boolean isBox() {
	// kryteria:
	// 1. cztery punkty musza miec pochodne
	//    bliskie jednosci (> 0.8)
	// 2. pozostale punkty musza miec pochodne
	//    bliskie zera (< 0.2)
	// 3. ostatni punkt musi byc blisko
	//    pierwszego
	// 4  wspolrzedne punktow musza miec
	//
	int l = points.size();
	if (l < 4) {
	    log.log("too few points for a box");
	    return false;
	}

	Point pl1 = points.get(l-1).point;
	Point p0 = points.get(0).point;
	
	float d = pl1.distanceTo(p0);
	
	if (d > 100) {
	    log.log("distance between the first "
		    +p0+" to the last "+pl1
		    +" too big: "+d);
	    return false;
	}


	   
	ArrayList<DerivPoint> sorted =
	    new ArrayList(points);

	Collections
	    .sort(sorted,
		  new Comparator<DerivPoint>() {
		      public int
			  compare(DerivPoint p1,
				  DerivPoint p2) {
			  float d = (float)
			      (Math.abs(p2.deriv)
			       - Math.abs(p1.deriv));
			  return d < 0 ? -1
			      : d > 0 ? 1
			      : 0;
		      }
		  });
	
	for (int i = 0; i < l; ++i) {
	    log.log(i+": "+sorted.get(i).deriv);
	}

	for (int i = 0; i < 4; ++i) {
	    if (Math.abs(sorted.get(i).deriv) < 0.7) {
		log.log("no corners detected");
		return false;
	    }
	}
	
	if (l > 4
	    && Math.abs(points.get(4).deriv) > 0.6) {
	    log.log("too many corners detected");
	    return false;
	}
       
	for (int i = 0; i < 4; ++i) {
	    Point p1 = sorted.get(i).point;
	    float d1 = sorted.get(i).deriv;
	    
	    Point p2 = sorted.get((i+1)%4).point;
	    float d2 = sorted.get((i+1)%4).deriv;

	    if (i > 0 &&
		Math.signum(d1) != Math.signum(d2)) {
		log.log("derivative signs of pair "+i
			+" differ");
		return false;
	    }

	    float dx = (float) Math.abs(p1.x-p2.x);
	    float dy = (float) Math.abs(p1.y-p2.y);
	    if(dx > 300 && dy > 300) {
		log.log("invalid corner positions "
			+i+" "+p1+" "+p2+", dx="+dx
			+", dy="+dy);
		return false;
	    }
	}
		
	return true; //false;
    }
}
