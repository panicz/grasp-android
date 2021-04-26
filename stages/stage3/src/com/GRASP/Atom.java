
package com.GRASP;
import android.graphics.Canvas;


class Atom extends Bit {
    /*@NonNull*/ public String text;

    @Override
    public void render(Canvas canvas) {
	canvas.drawText(text, 0, GRASP.paint.getTextSize(),
			GRASP.paint);
    }
    
    @Override public float width() {
	return GRASP.paint.measureText(text);
    }
    
    @Override public float height() {
	return GRASP.paint.getTextSize();
    }
    
    public Atom(String value) {
	text = value;
    }
}
