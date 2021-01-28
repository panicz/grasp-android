package com.GRASP;
import android.graphics.Canvas;
import android.graphics.RectF;
//import java.lang.Math;


class Editor extends Interactions {

    Document document;

    float horizontal_scroll = 0.0f;
    float vertical_scroll = 0.0f;

    String id;

    static int instances = 0;

    @Override
    public String toString() {
	return id;
    }
    
    public Editor(float x, float y, float w, float h,
		  Document doc, float hscroll, float vscroll) {
	super(x, y, w, h);
	document = doc;
	horizontal_scroll = hscroll;
	vertical_scroll = vscroll;
	id = String.valueOf(++instances);
    }
    
    @Override
    public Interactions copy() {
	return new Editor(left, top, width, height,
			  document,
			  horizontal_scroll,
			  vertical_scroll);
    }
    
    @Override
    public boolean canBeSplittedVerticallyBy(RectF line) {
	return top < line.top
	    && line.bottom < top+height
	    && line.left - left < near_edge
	    && (left+width) - line.right < near_edge;
    }

    @Override
    public boolean canBeSplittedHorizontallyBy(RectF line) {
	return left < line.left
	    && line.right < left+width 
	    && line.top - top < near_edge
	    && (top+height) - line.bottom < near_edge;
    }

    @Override
    public Interactions splitHorizontallyBy(RectF line) {
	GRASP.log("splitting "+id+ " horizontally");
	return new HorizontalSplit(this, line);
    }

    @Override
    public Interactions splitVerticallyBy(RectF line) {
	GRASP.log("splitting "+id+ " vertically");

	return new VerticalSplit(this, line);
    }

    @Override
    public void scrollBy(float x, float y) {
	horizontal_scroll += x;
	vertical_scroll += y;
    }
    
    @Override
    public void render(Canvas canvas) {
	String pos = "("+(int)left+", "+(int)top+")";
	GRASP.paint.setTextSize(18);
	canvas.drawText(pos,
			left + width/2.0f - 6*pos.length(),
			top + height/2.0f - 36,
			GRASP.paint);
	
	GRASP.paint.setTextSize(36);
	canvas.drawText(id,
			left + width/2.0f,
			top + height/2.0f,
			GRASP.paint);

	String size = "("+(int)width+", "+(int)height+")";
	GRASP.paint.setTextSize(18);
	canvas.drawText(size,
			left + width/2.0f,
			top + height/2.0f + 36,
			GRASP.paint);
    }
}
