package com.GRASP;

import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Typeface;
import android.graphics.Color;

class TextLine extends Widget {
    String text;
    Paint paint;
    public static Paint default_paint = null;

    static Paint getDefaultPaint() {
	if (default_paint == null) {
	    Typeface font = Typeface
		.createFromAsset(Utils.assets,
				 "DroidSans.ttf");
	    default_paint = new Paint();
	    default_paint.setTypeface(font);
	    default_paint.setTextSize(48);
	    default_paint.setColor(Color.BLACK);
	}
	return default_paint;
    }
    
    public TextLine(String text, Paint paint) {
	this.text = text;
	this.paint = paint;
    }

    public TextLine(String text) {
	this(text, getDefaultPaint());
    }
    
    @Override
    public void render(Canvas canvas,
		       float clip_left, float clip_top,
		       float clip_width, float clip_height) {
	canvas.drawText(text, 0, paint.getTextSize(), paint);
    }
    //Skim skim(float x, float y);

    @Override
    public float width() {
	return paint.measureText(text);
    }

    @Override
    public float height() {
	return paint.getTextSize();
    }

    @Override
    public String toString() {
	return text;
    }
    
}
