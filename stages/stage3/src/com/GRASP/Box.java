
package com.GRASP;

import android.graphics.Canvas;
//import android.graphics.Path;
import java.lang.StringBuilder;
import java.lang.Math;

class Box implements Bit {

    public Interline first_interline = null;
    
    final static float parenWidth = 20;
    final static float parenBar = 20;

    public static final float min_height =
	Atom.text_size + 2*Atom.vertical_margin;

    protected Space _following_space = null;

    static final Shift shift = new Shift();
    
    @Override
    public Space following_space() {
	return _following_space;
    }

    @Override
    public void set_following_space(Space s) {
	_following_space = s;
    }
    
    @Override
    public StringBuilder buildString(StringBuilder result) {
	result.append('(');

	for (Interline interline = first_interline;
	     interline != null;
	     interline = interline.following_line.next_interline) {
	    if(interline.following_line == null) {
		break;
	    }
	    
	    Line line = interline.following_line;

	    for (Space preceding_space = line.first_space;
		 preceding_space != null
		     && preceding_space.following_bit != null;
		 preceding_space = preceding_space
		     .following_bit
		     .following_space()) {
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
	
	for (Interline interline = first_interline;
	     interline != null;
	     interline = interline.following_line
		 .next_interline) {

	    /*
	    canvas.drawRect(20,accumulated_height,
			    40,accumulated_height+interline.height,
			    GRASP.paint);
	    */
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
		     preceding_space
		     .following_bit
		     .following_space()) {
		/*
		canvas.drawRect(accumulated_width,
				accumulated_height,
				accumulated_width
				+preceding_space.width,
				accumulated_height+20,
				GRASP.paint);
		*/
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

		//canvas.clipRect(0,0,w,h+100);
		bit.render(canvas);
		canvas.restore();
		
		accumulated_width += w;
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
	canvas.translate(w+parenWidth, 0);

	Paren.Right.render(canvas, GRASP.paint, h);

	canvas.translate(-(w+parenWidth), 0);
	GRASP.paint.setColor(previous_color);
    }

    
    @Override
    public float width() {
	return 2*parenWidth+first_interline.maximum_width();
    }
    
    @Override public float height() {
	return Math.max(min_height,
			first_interline.onward_height());
    }

    // used in the public dragAround below, overrode by Document
    protected Drag dragAround() {
	return new DragAround(this, 0, 0);
    }


    // used in the public dragAround below, overrode by Document
    protected Drag resize() {
	return new Resize(this);
    }

    
    @Override
    public Drag dragAround(float x, float y, TakeBit take) {
	float accumulated_height = 0;
	float maximum_width = 0;

	if (x < parenWidth) {
	    return dragAround();
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
		     preceding_space
		     .following_bit
		     .following_space()) {

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
		    Drag nested = bit.dragAround(rx, ry, take);
		    if (nested != null) {
			if (nested instanceof DragAround
			    && ((DragAround)nested).target == bit) {
			    ((DragAround)nested).target =
				take.from(preceding_space);
			}
			shift.set(accumulated_width,
				  accumulated_height);
			return nested.outwards(shift);
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
	    return resize();
	    //return dragAround();
	}
	return null;
    }

    // used from insertAt method, overrode by Document
    protected boolean insertLast(Interline last_interline,
				 Line line,
				 float x, float y,
				 float accumulated_height,
				 DragAround target) {
	return false;
    }
    
    @Override
    public boolean insertAt(float x, float y, DragAround target) {
	
	float accumulated_height = 0;
	Interline interline;
	Line line = null;
	
	for (interline = first_interline;
	     interline != null;
	     interline = interline.following_line.next_interline) {
	    
	    accumulated_height += interline.height;

	    if (y < accumulated_height) {
		return interline.insert_line_with(target);
	    }

	    
	    if(interline.following_line == null) {
		break;
	    }
	    
	    line = interline.following_line;

	    float line_height = line.height();

	    if (y >= accumulated_height + line_height) {
		accumulated_height += line_height;
		continue;
	    }
	    
	    float accumulated_width = parenWidth;
	    
	    for (Space last_space = line.first_space;
		 last_space != null;
		 last_space =
		     last_space.following_bit.following_space()) {

		if (x <= accumulated_width + last_space.width
		    || last_space.following_bit == null
		    ) {
		    shift.set(accumulated_width, accumulated_height);
		    return last_space
			.insertAt(x - accumulated_width,
				  y - accumulated_height,
				  (DragAround) target
				  .inwards(shift));
		}

		accumulated_width += last_space.width;
	       
		Bit bit = last_space.following_bit;
	       
		if (bit == null) {
		    break;
		}
		
		float w = bit.width();
		float h = bit.height();

		float rx = x - accumulated_width;
		float ry = y - accumulated_height;
		
		if (0 <= rx && rx <= w
		    && 0 <= ry && ry <= h) {
		    shift.set(accumulated_width, accumulated_height);
		    return bit
			.insertAt(rx, ry,
				  (DragAround) target
				  .inwards(shift));
		}

		accumulated_width += w;
		rx -= w;
		
		if (bit.following_space() == null) {
		    bit.set_following_space(new Space(rx));
		    shift.set(accumulated_width, accumulated_height);
		    return bit.following_space()
			.insertAt(rx, ry,
				  (DragAround) target
				  .inwards(shift));
		}
	    }
	    
	    accumulated_height += line_height;
	}

	return insertLast(interline, line, x, y,
			  accumulated_height, target);
    }

    public Bit shallow_copy() {
	Box copy = new Box();
	copy.first_interline = first_interline;
	return copy;
    }

    public Bit deep_copy() {
	Box copy = new Box();
	if(first_interline != null) {
	    copy.first_interline = first_interline.deep_copy();
	}
	
	if (_following_space != null) {
	    copy.set_following_space(_following_space.deep_copy());
	}

	return copy;	
    }

}
