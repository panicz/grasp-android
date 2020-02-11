package com.slayer;

import java.io.Serializable;
import java.lang.Math;

class Transform implements Serializable {
    Vector origin = new Vector(0, 0);
    public Vector offset = new Vector(0, 0);
    public float scale = 1.0f;

    float rotation = 0.0f;
    float sin = 0.0f;
    float cos = 1.0f;
    float inverse_scale = 1.0f;

    Logger log;

    public Transform(Logger log) {
	this.log = log;
    }
    

    void setScale(float s) {
	scale = s;
	inverse_scale = 1.0f/s;
    }

    void setRotation(float rad) {
	rotation = rad;
	sin = (float) Math.sin(rotation);
	cos = (float) Math.cos(rotation);
    }


    public Point p(Point p) {
	return new Point(scale*(cos*p.x - sin*p.y
				+ offset.x),
			 scale*(sin*p.x + cos*p.y
				+ offset.y));
    }

    Vector zero = new Vector(0, 0);
    
    public Point p(Point p, float scale,
		   float rotation,
		   Vector offset) {
	return new Point((float)
			 (scale*(Math.cos(rotation)*p.x
				- Math.sin(rotation)*p.y
				 + offset.x)),
			 (float)
			 (scale*(Math.sin(rotation)*p.x
				+ Math.cos(rotation)*p.y
				 + offset.y)));
    }

    public Point p(Point pt, float scale,
		   float rotation) {
	return p(pt, scale, rotation, zero);
    }

    int adjustments = 0;

    public void clear() {
	adjustments = 0;
    }
    
    public Point unp(Point p) {
	float X = inverse_scale*p.x - offset.x;
	float Y = inverse_scale*p.y - offset.y;
	return new Point(cos*X + sin*Y,
			 -sin*X + cos*Y);
    }
    /*

    p10_scr = s*(R * p1_doc + d);
    p11_scr = s'*(R' * p1_doc + d');
    p20_scr = s*(R * p2_doc + d);
    p21_scr = s'*(R' * p2_doc + d');
	
   
    R^T(s^-1*p10_scr - d) = p1_doc;
    R'^T(s'^-1*p11_scr - d') = p1_doc;

    R^T(s^-1*p20_scr - d) = p2_doc;
    R'^T(s'^-1*p21_scr - d') = p2_doc;


    R^T(s^-1*p10_scr - d) = R'^T(s'^-1*p11_scr - d');
    R^T(s^-1*p20_scr - d) = R'^T(s'^-1*p21_scr - d');

    R'*p2_doc = s'^-1*p21_scr - d';
    R'*p1_doc = s'^-1*p11_scr - d';

    R'*(p2_doc-p1_doc) = s'^-1*(p21_scr - p11_scr);
    
    R'*dp_doc = s'^-1*v1;
    
    cos'*dp_doc.x - sin'*dp_doc.y = s'^-1*v1.x;
    sin'*dp_doc.x + cos'*dp_doc.y = s'^-1*v1.y;
    s'^-1*(v1.x+v1.y) = cos'*(dp_doc.x+dp_doc.y)
	+ sin'*(dp_doc.x-dp_doc.y);
    s'^-1*(v1.x-v1.y) = cos'*(dp_doc.x-dp_doc.y)
	- sin'*(dp_doc.x+dp_doc.y)
    V = cos'*P + sin'*N;
    W = cos'*N - sin'*P;
    cos' = (sin'*P + W)/N;
    V = (sin'*P + W)/N + sin'*N;
    V*N = sin'*P + W + sin'*N²;
    V*N-W = sin'*(P+N²);
    sin' = (V*N - W)/(P+N*N);
    cos' = ((V*N - W)/(P+N*N)*P + W)/N;

    P = dp_doc.x+dp_doc.y;
    N = dp_doc.x-dp_doc.y;
    V = s'^-1*(v1.x+v1.y);
    W = s'^-1*(v1.x-v1.y);
    
   
    cos'(dp_doc.x - dp_doc.y) 
    - sin'(dp_doc.x + dp_doc.y)
    = s'^-1*(v1.x - v1.y);
    
    cos'(dp_doc.x + dp_doc.y)
    + sin'*(dp_doc.x - dp_doc.y)  
    = s'^-1(v1.x +  v1.y);

    cos*P + sin*N = A
    cos*N - sin*P = B



        R'*p1_doc = s'^-1*p11_scr - d';
    d' = s'^-1*p11_scr - R'*p1_doc;
    d' = s'^-1*p21_scr - R'*p2_doc;
    */
    Vector offset1 = new Vector(0, 0);

    Vector offset2 = new Vector(0, 0);

    float interpolate(float lo, float hi, int percent) {
	if (percent <= 0) {
	    return lo;
	}
	if (percent >= 100) {
	    return hi;
	}
	return lo + percent*(hi-lo)/100;
	
    }
    
    public Point [] adjusted(Point p10_scr,
			     Point p11_scr,
			     Point p20_scr,
			     Point p21_scr) {
	/*
	log.log(p10_scr.toString()+" to "+
		p11_scr.toString()+", "+
		p20_scr.toString()+" to "+
		p21_scr.toString());*/
	Vector v0 = new Vector(p10_scr, p20_scr);
	Vector v1 = new Vector(p11_scr, p21_scr);
	if (Math.abs(v1.x) < 1) {
	    return null;
	}

	float d0 = v0.length();
	if (d0 < 1) {
	    return null;
	}
	float d1 = v1.length();
	if (d1 < 1) {
	    return null;
	}

	Point p1_doc = unp(p10_scr);
	Point p2_doc = unp(p20_scr);

	Vector dp_doc = new Vector(p1_doc,
				   p2_doc);
	
	float s = d1/d0;
	
	float scale2 = scale*s;

	/*
	log.log("new scale = "+scale2
	+" (previous: "+scale+")");*/
	
	float P = dp_doc.x+dp_doc.y;
	float N = (dp_doc.x-dp_doc.y);
	float NN = N*N;
	float V = (v1.x+v1.y)/scale2;
	float VN = V*N;
	float W = (v1.x-v1.y)/scale2;

	//float cos2 = (A - B/P)/(P - N/P);
	//float sin2 = (B - N*cos2)/P;


	float sin2 = (VN-W)/(P+NN);
	float cos2 = (sin2*P + W)/N;

	float rotation2 =
	    (float) Math.atan2(sin2, cos2);

	/*
	log.log("new rotation = "+rotation2
	+" (previous: "+rotation+")");*/

	/*
	offset.set(p21_scr.x/scale2
		    - (cos2*p2_doc.x - sin2*p2_doc.y),
		    p21_scr.y/scale2
		    - (sin2*p2_doc.x + cos2*p2_doc.y));*/

	//log.log("ofs1: "+offset1+", ofs2: "+offset2);
	
	offset1.set(p11_scr.x/scale2
		    - (cos2*p1_doc.x - sin2*p1_doc.y),
		    p11_scr.y/scale2
		    - (sin2*p1_doc.x + cos2*p1_doc.y));

	
	Point p11_scr_ = p(p1_doc, scale2, rotation2,
			   offset1);

	
	Point p21_scr_ = p(p2_doc, scale2, rotation2,
			   offset1);

	//Vector fix1 = new Vector(p11_scr+" vs "+p11_scr_

	float p1d = p11_scr.distanceTo(p11_scr_);
	float p2d = p21_scr.distanceTo(p21_scr_);

	/*
	if (p1d > 2 || p2d > 2) {	
	    log.log(p11_scr+" vs "+p11_scr_
		    +"  ("+p1d+")"); 
	    log.log(p21_scr+" vs "+p21_scr_
		    +"  ("+p2d+")"); 
	    //return null;
	    }*/

	
	setScale(interpolate(scale, scale2, adjustments));

	setRotation(interpolate(rotation, rotation2,
				adjustments));

	offset.set(interpolate(offset.x,
			       offset1.x,
			       adjustments),
		   interpolate(offset.y,
			       offset1.y,
			       adjustments)); 
	adjustments += 10;


	/*
	  to nie dziala, bo sinusy albo kosinusy
	  potrafia byc weksze od 1
	rotation = rotation2;
	sin = sin2;
	cos = cos2;*/
	
	/*
	log.log("sc="+scale2+", ro="+rotation2
		+", ("+sin2+", "+cos2+"), ofs="
		+offset.toString());
	*/

	
	Point result[] = { p11_scr_, p21_scr_ };
	return result;
    }
    
}
