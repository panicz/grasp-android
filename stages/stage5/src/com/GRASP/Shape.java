package com.GRASP;

import android.graphics.RectF;

//import java.io.Serializable;
//import java.lang.Math;
import java.util.ArrayList;
import java.util.List;

//import java.util.Comparator;
//import java.util.Collections;

//import android.graphics.Color;
import android.graphics.Canvas;
//import android.graphics.Paint;

class Shape {
    public List<Stroke> strokes = new ArrayList<Stroke>();

    public RectF area = new
	RectF(Float.POSITIVE_INFINITY,
	      Float.POSITIVE_INFINITY,
	      Float.NEGATIVE_INFINITY,
	      Float.NEGATIVE_INFINITY);

    public void clear() {
	strokes.clear();
	area.left = Float.POSITIVE_INFINITY;
	area.top = Float.POSITIVE_INFINITY;
	area.right = Float.NEGATIVE_INFINITY;
	area.bottom = Float.NEGATIVE_INFINITY;
    }

    public void add(Stroke stroke) {
	if (stroke.rect.left < area.left) {
	    area.left = stroke.rect.left;
	}
	if (stroke.rect.right > area.right) {
	    area.right = stroke.rect.right;
	}
	if (stroke.rect.top < area.top) {
	    area.top = stroke.rect.top;
	}
	if (stroke.rect.bottom > area.bottom) {
	    area.bottom = stroke.rect.bottom;
	}
    }

    public float getWidth() {
	return area.right - area.left;
    }

    public float getHeight() {
	return area.bottom - area.top;
    }

    public void draw(Canvas canvas) {
	for (Stroke stroke : strokes) {
	    stroke.draw(canvas);
	}
    }
    
}

