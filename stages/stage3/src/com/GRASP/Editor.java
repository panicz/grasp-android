package com.GRASP;
import android.graphics.Canvas;
import android.graphics.RectF;
//import java.lang.Math;


class Editor extends Panel {

    Document document;

    float horizontal_scroll = 0.0f;
    float vertical_scroll = 0.0f;
    float scale = 1.0f;
    
    String id;

    static int instances = 0;
    
    @Override
    public String toString() {
	return id;
    }

    public boolean is_pinned = false;
    
    public Editor(float x, float y, float w, float h,
		  Document doc, float hscroll, float vscroll) {
	super(x, y, w, h);
	document = doc;
	horizontal_scroll = hscroll;
	vertical_scroll = vscroll;
	id = String.valueOf(++instances);
	
	// powinnismy zwracac opcje dla dokumentu
	// albo ktoregos jego elementu

    }
    
    @Override
    public Panel copy() {
	return new Editor(left(), top(), width(), height(),
			  document,
			  horizontal_scroll,
			  vertical_scroll);
    }
    
    @Override
    public boolean canBeSplittedVerticallyBy(RectF line) {
	return top() < line.top
	    && line.bottom < bottom()
	    && line.left - left() < near_edge
	    && right() - line.right < near_edge;
    }

    @Override
    public boolean canBeSplittedHorizontallyBy(RectF line) {
	return left() < line.left
	    && line.right < right() 
	    && line.top - top() < near_edge
	    && bottom() - line.bottom < near_edge;
    }

    @Override
    public Panel splitHorizontallyBy(RectF line) {
	return new HorizontalSplit(this, line);
    }

    @Override
    public Panel splitVerticallyBy(RectF line) {
	return new VerticalSplit(this, line);
    }

    @Override
    public void scrollBy(float x, float y) {
	horizontal_scroll += x;
	vertical_scroll += y;
    }
    
    @Override
    public void render(Canvas canvas,
		       float clip_left, float clip_top,
		       float clip_width, float clip_height) {
	String pos = "("+(int)left()+", "+(int)top()+")";
	GRASP.paint.setTextSize(18);
	canvas.drawText(pos,
			left() + width()/2.0f - 6*pos.length(),
			top() + height()/2.0f - 36,
			GRASP.paint);
	
	GRASP.paint.setTextSize(36);
	canvas.drawText(id,
			left() + width()/2.0f,
			top() + height()/2.0f,
			GRASP.paint);

	String size = "("+(int)width()+", "+(int)height()+")";
	GRASP.paint.setTextSize(18);
	canvas.drawText(size,
			left() + width()/2.0f,
			top() + height()/2.0f + 36,
			GRASP.paint);
    }

    @Override
    public Drag onPress(Layers layers,
			int finger,
			float x, float y) {
	return null;
	/*
	Location source = document
	    .locationOfElementAtPosition
	    (x + horizontal_scroll,
	     y + vertical_scroll);
	if (source == null) {
	    return null;
	}

	Element target = document
	    .takeElementFromLocation(source);

	return new MoveAround(layers.add(target));
	*/
    }

    
}
