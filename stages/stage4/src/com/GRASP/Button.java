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

    
    public Button(String label, float w, float h) {

	if (paint == null) {
	    paint = new Paint();
	    paint.setStrokeWidth(4);
	    paint.setTypeface(GRASP.menu_font);
	    paint.setTextSize(text_size);
	    paint.setFlags(Paint.ANTI_ALIAS_FLAG);
	}
	
	caption = label;
	trySetSize(w, h);
    }
    
    @Override
    public void render(Canvas canvas) {
	paint.setColor(0xeeffffff);
	
	canvas.drawRoundRect(0.0f, 0.0f,
			     _width, _height,
			     5.0f, 5.0f, paint);
	
	paint.setColor(0xee000000);
	
	canvas.drawText(caption, horizontal_margin,
			(height()
			 + text_size
			 - vertical_margin)/2,
			paint);
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
			  + 2*horizontal_margin);
	_height = Math.max(y, text_size+2*vertical_margin);
    }
    
    @Override
    public Drag onPress(Screen screen,
			byte finger,
			float x, float y) {
	// jezeli wcisnelismy na ramke u gory
	// to przeciagamy wyrazenie,
	// u dolu - byc moze zmieniamy jego rozmiar
	return null;
    }

    @Override
    public void onClick(Screen screen,
			byte finger,
			float x, float y) {
	
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
}
