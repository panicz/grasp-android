package com.GRASP;

import android.graphics.Canvas;
import android.graphics.Paint;

import java.lang.Math;

class Button implements Pad {

    float _width, _height;
    String caption;
    public static final float text_size = 56;
    static Paint paint = null;

    public static final float horizontal_margin = 8;
    public static final float vertical_margin = 20;

    public final int BGCOLOR = 0xccffffff;
    public final int FGCOLOR = 0xcc000000;
    
    public int bgcolor = BGCOLOR;
    public int fgcolor = FGCOLOR;

    static Noop noop = new Noop();
    Action action;
    
    public Button(String label, float w, float h,
		  Action whenClicked) {
	if (paint == null) {
	    paint = new Paint();
	    paint.setStrokeWidth(4);
	    paint.setTypeface(GRASP.menu_font);
	    paint.setTextSize(text_size);
	    paint.setFlags(Paint.ANTI_ALIAS_FLAG);
	}
	
	caption = label;
	trySetSize(w, h);
	action = whenClicked;
    }

    public Button(String label, float w, float h) {
	this(label, w, h, noop);
    }

    public Button(String label, Action whenClicked) {
	this(label, 0, 0, whenClicked);
    }

    public Button(String label) {
	this(label, 0, 0, noop);
    }

    
    @Override
    public void render(Canvas canvas) {
	paint.setColor(bgcolor);
	
	canvas.drawRoundRect(1.0f, 1.0f,
			     _width-2.0f, _height-2.0f,
			     5.0f, 5.0f, paint);
	
	paint.setColor(fgcolor);
	
	canvas.drawText(caption, horizontal_margin,
			(height()
			 + text_size
			 - vertical_margin)/2,
			paint);
    }

    void invert_colors() {
	fgcolor = BGCOLOR;
	bgcolor = FGCOLOR;
    }

    void normal_colors() {
	bgcolor = BGCOLOR;
	fgcolor = FGCOLOR;
    }
    
    @Override
    public float width() {
	return _width;
    }

    @Override
    public float height() {
	return _height;
    }

    @Override
    public void trySetSize(float x, float y) {
	_width = Math.max(x, paint.measureText(caption)
			  + 3*horizontal_margin);
	_height = Math.max(y, text_size+2*vertical_margin);
    }

    
    @Override
    public Drag onPress(Screen screen,
			byte finger,
			float x, float y) {
	invert_colors();
	return null;
    }
    
    @Override
    public void onClick(Screen screen,
			byte finger,
			float x, float y) {
	action.perform(finger, x, y);
    }

    @Override
    public Drag onSecondPress(Screen screen,
			      byte finger,
			      float x, float y) {
	return null;
    }

    @Override
    public void onDoubleClick(Screen screen,
			      byte finger,
			      float x, float y) {

    }

    @Override
    public Drag onHold(Screen screen,
		       byte finger,
		       float x, float y) {
	return null;
    }

    @Override
    public void onDragOver(Screen screen, byte finger,
			   float x, float y) {
	//GRASP.log("over "+caption);
	invert_colors();
    }

    @Override
    public void onDragOut(Screen screen, byte finger) {
	//GRASP.log("out "+caption);
	normal_colors();
    }

    @Override
    public void onRelease(Screen screen, byte finger,
			  float x, float y) {
	action.perform(finger, x, y);
    }

    
}
