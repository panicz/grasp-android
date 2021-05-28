package com.GRASP;
//import android.graphics.Canvas;
import android.graphics.RectF;

abstract class Panel implements Widget {
    static final float near_edge = 60;

    static int stretches = 0;
    
    protected float _left;
    protected float _top;
    protected float _width;
    protected float _height;
    protected float _right;
    protected float _bottom;

    //@Deprecated
    public float left() { return _left; }
    
    //@Deprecated
    public float top() { return _top; }
    
    @Override
    public float width() {
	return _width;
    }
    
    @Override
    public float height() {
	return _height;
    }

    //@Deprecated
    public float right() { return _right; }
    
    //@Deprecated
    public float bottom() { return _bottom; }

    public void setLeft(float v) {
	_left = v;
	_right = _left + _width;
    }

    public void setTop(float v) {
	_top = v;
	_bottom = _top + _height;
    }

    public void setWidth(float v) {
	_width = v;
	_right = _left + _width;
    }

    public void setHeight(float v) {
	_height = v;
	_bottom = _top + _height;
    }    
    
    public Panel(float x, float y, float w, float h) {	_left = x;
	_top = y;
	_width = w;
	_height = h;
	_right = x+w;
	_bottom = y+h;
    }

    public abstract Panel copy();
    
    public abstract boolean
	canBeSplittedVerticallyBy(RectF line);

    public abstract boolean
	canBeSplittedHorizontallyBy(RectF line);
    
    public Drag onPress(Screen screen,
			int finger,
			float x, float y) {
	return null;
    }

    public void onClick(Screen screen,
			int finger,
			float x, float y) {}

    public Drag onSecondPress(Screen screen,
			      int finger,
			      float x, float y) {
	return null;
    }

    public void onDoubleClick(Screen screen,
			      int finger,
			      float x, float y) {
    }

    public Drag onHold(Screen screen,
		       int finger,
		       float x, float y) {
	return null;
    }
    
    public abstract void scrollBy(float x, float y);
    
    public abstract Panel
	splitHorizontallyBy(RectF line);

    public abstract Panel
	splitVerticallyBy(RectF line);
    
    public Panel
	finishResizing(Split s, float vx, float vy) {
	return this;
    }

    public Panel at(float x, float y) {
	return this;
    }

    public abstract Drag stretchFrom(int finger, float x, float y);
    
    public abstract void stretch();

    public abstract boolean insertAt(float x, float y,
				     DragAround bit);

    protected Drag translate(Drag drag, float x, float y) {
	if (drag == null) {
	    return null;
	}
	return drag.translate(x, y);
    }
};
