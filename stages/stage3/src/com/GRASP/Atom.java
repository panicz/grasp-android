
package com.GRASP;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Color;


class Atom extends Bit {
    public static final float horizontal_margin = 8;
    public static final float vertical_margin = 20;


    public static final float text_size = 72;
    static Paint paint = null;
    
    /*@NonNull*/ public String text;

    @Override
    public void render(Canvas canvas) {
	GRASP.paint.setTypeface(GRASP.symbols_font);
	GRASP.paint.setTextSize(text_size);

	canvas.drawRoundRect(0.0f, 30.0f,
			     width(), height()-30.0f,
			     15.0f, 15.0f, paint);
	canvas.drawText(text, horizontal_margin,
			(height()
			 + GRASP.paint.getTextSize()
			 - vertical_margin)/2,
			GRASP.paint);
    }
    
    @Override public float width() {
	return paint.measureText(text) + 2*horizontal_margin;
    }
    
    @Override public float height() {
	return text_size + 2*vertical_margin;
    }
    
    public Atom(String value) {
	text = value;
	if (paint == null) {
	    paint = new Paint();
	    paint.setColor(0xffeeeeee);
	    paint.setStrokeWidth(4);
	    paint.setTypeface(GRASP.symbols_font);
	    paint.setTextSize(text_size);
	}
    }

    @Override
    public String toString() {
	return text;
    }

    @Override
    protected StringBuilder buildString(StringBuilder result) {
	result.append(text);
	return result;
    }

    @Override
    public DragAround dragAround(float x, float y) {
	return new DragAround(this, 0, 0);
    }
    
}
