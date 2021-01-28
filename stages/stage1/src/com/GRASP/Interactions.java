package com.GRASP;
import android.graphics.Canvas;
import android.graphics.RectF;

abstract class Interactions {
    static final float near_edge = 60;
        
    public float left;
    public float top;
    public float width;
    public float height;
    
    public Interactions(float x, float y, float w, float h) {
	left = x;
	top = y;
	width = w;
	height = h;
    }

    public abstract Interactions copy();
    
    public abstract boolean
	canBeSplittedVerticallyBy(RectF line);

    public abstract boolean
	canBeSplittedHorizontallyBy(RectF line);
    
    public Split splitUnder(float x, float y) {
	return null;
    }

    public abstract void scrollBy(float x, float y);
    
    public abstract Interactions
	splitHorizontallyBy(RectF line);

    public abstract Interactions
	splitVerticallyBy(RectF line);
    
    public abstract void render(Canvas canvas);

    public abstract String toString();
};
