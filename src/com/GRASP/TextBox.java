package com.GRASP;

import android.graphics.Canvas;
import android.graphics.RectF;

import android.graphics.Paint;
import android.graphics.Typeface;

import java.lang.Math;

class TextBox extends IdleBox {

    Paint paint;
    String text;

    float horizontal_margin;
    float vertical_margin;

    float text_width;
    float text_height;

    float min_width;
    float min_height;

    
    float total_width;
    float total_height;

    public void setText(String text) {
	this.text = text;

	text_width = paint.measureText(text);
	text_height = paint.getTextSize();
	total_width = Math.max(text_width, min_width);
	total_height = Math.max(text_height, min_height);

	vertical_margin = total_height - text_height;
	horizontal_margin = total_width - text_width;
    }
    
    public TextBox(String text,
		   Typeface font,
		   int fontsize,
		   int color,
		   float min_width,
		   float min_height) {

	paint = new Paint();
	paint.setTypeface(font);
	paint.setTextSize(fontsize);
	paint.setColor(color);
	paint.setStrokeWidth(4);
	this.min_width = min_width;
	this.min_height = min_height;
	setText(text);
    }
    
    @Override
    public void draw(Canvas canvas) {
	canvas.drawText(text,
			horizontal_margin/2,
			text_height+vertical_margin/2,
			paint);
    }
    
    @Override
    public float getWidth() {
	return total_width;
    }

    @Override
    public float getHeight() {
	return total_height;
    }

    TouchHandler singleTapHandler = impassive;
    
    @Override
    public ActionResult onSingleTap(float x, float y) {
	return singleTapHandler.action(x, y);
    }

    TouchHandler doubleTapHandler = impassive;

    @Override
    public ActionResult onDoubleTap(float x, float y) {
	return singleTapHandler.action(x, y);
    }

    TouchHandler holdHandler = impassive;

    @Override
    public ActionResult onHold(float x, float y) {	
	return holdHandler.action(x, y);
    }

    @Override
    public boolean contains(float x, float y) {
	//GRASP.Log("checking "+x+", "+y+" against "+text);
	return 0 < x && x < total_width
	    && 0 < y && y < total_height;
    }

}
