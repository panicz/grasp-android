package com.GRASP;

import android.graphics.Canvas;
import android.graphics.Path;
import java.lang.StringBuilder;

class Box extends Bit {

    /*@NonNull*/ public Interline first_interline = null;

    final static float parenWidth = 20;
    final static float parenBar = 20;

    private float accumulated_height;
    private float maximum_width;

    @Override
    protected StringBuilder buildString(StringBuilder result) {
	result.append('(');
	Interline interline;
	for (interline = first_interline;
	     interline != null;
	     interline = interline.following_line.next_interline) {
	    if(interline.following_line == null) {
		break;
	    }
	    
	    Line line = interline.following_line;

	    for (Space preceding_space = line.first_space;
		 preceding_space != null
		     && preceding_space.following_bit != null;
		 preceding_space =
		     preceding_space.following_bit.following_space) {
		preceding_space.following_bit.buildString(result);
		result.append(' ');
	    }
	}
	result.deleteCharAt(result.length() - 1);
	result.append(')');
	return result;
    }

    @Override
    public String toString() {
	return buildString(new StringBuilder()).toString();
    }
    
    public void renderContents(Canvas canvas) {
	float accumulated_height = 0;
	float maximum_width = 0;
	
	for (Interline interline = first_interline;
	     interline != null;
	     interline = interline.following_line.next_interline) {

	    accumulated_height += interline.height;

	    if(interline.following_line == null) {
		break;
	    }
	    
	    Line line = interline.following_line;

	    float line_height = line.height();
	    
	    float accumulated_width = parenWidth;
	    
	    for (Space preceding_space = line.first_space;
		 preceding_space != null
		     && preceding_space.following_bit != null;
		 preceding_space =
		     preceding_space.following_bit.following_space) {

		Bit bit = preceding_space.following_bit;
	       
		accumulated_width += preceding_space.width;

		float w = bit.width();
		float h = bit.height();

		canvas.save();
		canvas.translate(accumulated_width,
				 accumulated_height);
		canvas.clipRect(0,0,w,h+100);
		bit.render(canvas);
		canvas.restore();
		
		accumulated_width += w;
	    }
	    if (accumulated_width > maximum_width) {
		maximum_width = accumulated_width;
	    }
	    
	    accumulated_height += line_height;
	}

    }

    
    @Override
    public void render(Canvas canvas) {

	float w = first_interline.maximum_width();
	float h = first_interline.onward_height();
	
	int previous_color = GRASP.paint.getColor();
	GRASP.paint.setColor(previous_color + 0x111111);
	
	renderContents(canvas);
	
	Paren.Left.render(canvas, GRASP.paint, h);
	canvas.save();
	canvas.translate(w+parenWidth, 0);

	Paren.Right.render(canvas, GRASP.paint, h);
	
	canvas.restore();
	GRASP.paint.setColor(previous_color);
    }

    //@Override
    public Drag onPress(Screen screen,
			int finger,
			float x, float y) {
	return null;
    }
    
    @Override public float width() {
	return 2*parenWidth+first_interline.maximum_width();
    }
    
    @Override public float height() {
	return first_interline.onward_height();
    }

    @Override
    public Bit itemAt(float x, float y) {
	
	float accumulated_height = 0;
	float maximum_width = 0;
	//Interline interline;
	
	for (Interline interline = first_interline;
	     interline != null;
	     interline = interline.following_line.next_interline) {

	    accumulated_height += interline.height;

	    if(interline.following_line == null) {
		break;
	    }
	    
	    Line line = interline.following_line;

	    float line_height = line.height();
	    
	    float accumulated_width = parenWidth;
	    float maximum_height = 0;
	    
	    for (Space preceding_space = line.first_space;
		 preceding_space != null
		     && preceding_space.following_bit != null;
		 preceding_space =
		     preceding_space.following_bit.following_space) {

		Bit bit = preceding_space.following_bit;
	       
		accumulated_width += preceding_space.width;

		float w = bit.width();
		float h = bit.height();

		assert(h <= line_height);
		
		if (h > maximum_height)  {
		    maximum_height = h;
		}

		float rx = x - accumulated_width;
		float ry = y - accumulated_height;
		
		if (0 <= rx && rx <= w
		    && 0 <= ry && ry <= h) {
		    Bit result = bit.itemAt(rx, ry);
		    if (result == null) {
			return this;
		    }
		    return result;
		}
		
		accumulated_width += w;
	    }
	    
	    if (accumulated_width > maximum_width) {
		maximum_width = accumulated_width;
	    }
	    
	    accumulated_height += maximum_height;
	}


	return this;
    }

    @Override
    public boolean insertAt(float x, float y, Bit item) {
	return false;
    }

    @Override
    public Bit takeFrom(float x, float y) {
	return this;
    }

}
