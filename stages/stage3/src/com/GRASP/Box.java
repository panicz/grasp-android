package com.GRASP;

import android.graphics.Canvas;
import android.graphics.Path;
import java.lang.StringBuilder;
import java.lang.Math;

class Box extends Bit {

    /*@NonNull*/ public Interline first_interline = null;

    final static float parenWidth = 20;
    final static float parenBar = 20;

    private float accumulated_height;
    private float maximum_width;

    public static final float min_height =
	Atom.text_size + 2*Atom.vertical_margin;
    
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
		 preceding_space != null;
		 preceding_space =
		     preceding_space.following_bit.following_space) {

		accumulated_width += preceding_space.width;
		
		Bit bit = preceding_space.following_bit;
	       
		if (bit == null) {
		    break;
		}
		
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
	float h = height();
	
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

    
    @Override public float width() {
	return 2*parenWidth+first_interline.maximum_width();
    }
    
    @Override public float height() {
	return Math.max(min_height,
			first_interline.onward_height());
    }

    @Override
    public DragAround dragAround(float x, float y) {
	float accumulated_height = 0;
	float maximum_width = 0;

	if (x < parenWidth) {
	    return new DragAround(this, 0, 0);
	}
	
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
		 preceding_space != null;
		 preceding_space =
		     preceding_space.following_bit.following_space) {

		accumulated_width += preceding_space.width;

		Bit bit = preceding_space.following_bit;

		if (bit == null) {
		    break;
		}
		
		float w = bit.width();
		float h = bit.height();

		assert(h <= line_height);
		
		float rx = x - accumulated_width;
		float ry = y - accumulated_height;
		
		if (0 <= rx && rx <= w
		    && 0 <= ry && ry <= h) {
		    DragAround nested = bit.dragAround(rx, ry);
		    if (nested != null) {
			if (nested.target == bit
			    && preceding_space.remove_following_bit()
			    .following_bit == null
			    && preceding_space == line.first_space) {
			    //interline.remove_following_line();
			}
			return (DragAround)
			    nested.translate(accumulated_width,
					     accumulated_height);
		    }
		    return null;
		}
		accumulated_width += w;
	    }
	    
	    if (accumulated_width > maximum_width) {
		maximum_width = accumulated_width;
	    }
	    accumulated_height += line_height;
	}
	if (maximum_width <= x && x < maximum_width + parenWidth) {
	    return new DragAround(this, 0, 0);
	}
	return null;
    }

    @Override
    public boolean insertAt(float x, float y, Bit target) {
       
	float accumulated_height = 0;
	float maximum_width = 0;
	
	for (Interline interline = first_interline;
	     interline != null;
	     interline = interline.following_line.next_interline) {
	    
	    accumulated_height += interline.height;

	    if (y < accumulated_height) {
		// ... chcemy dodac do interlinii
		return false;
	    }

	    
	    if(interline.following_line == null) {
		break;
	    }
	    
	    Line line = interline.following_line;

	    float line_height = line.height();

	    if (y >= accumulated_height + line_height) {
		accumulated_height += line_height;
		continue;
	    }
	    
	    float accumulated_width = parenWidth;
	    
	    for (Space preceding_space = line.first_space;
		 preceding_space != null;
		 preceding_space =
		     preceding_space.following_bit.following_space) {

		if (x <= accumulated_width + preceding_space.width) {
		    return preceding_space
			.insertAt(x - accumulated_width,
				  y - accumulated_height,
				  target);
		}
		
		accumulated_width += preceding_space.width;
		
		Bit bit = preceding_space.following_bit;
	       
		if (bit == null) {
		    break;
		}
		
		float w = bit.width();
		float h = bit.height();

		float rx = x - accumulated_width;
		float ry = y - accumulated_height;
		
		if (0 <= rx && rx <= w
		    && 0 <= ry && ry <= h) {
		    return bit.insertAt(rx, ry, target);
		}
		
		accumulated_width += w;
	    }
	    if (accumulated_width > maximum_width) {
		maximum_width = accumulated_width;
	    }
	    
	    accumulated_height += line_height;
	}
	return false;
    }
    
    
}
