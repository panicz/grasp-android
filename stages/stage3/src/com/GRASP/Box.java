package com.GRASP;

import android.graphics.Canvas;
import android.graphics.Path;


class Box extends Bit {

    /*@NonNull*/ public Interline first_interline = null;

    final static float parenWidth = 20;
    final static float parenBar = 20;

    @Override
    public void render(Canvas canvas) {

	float accumulated_height = 0;

	Interline interline;

	float maximum_width = 0;

	int previous_color = GRASP.paint.getColor();
	GRASP.paint.setColor(previous_color + 0x111111);
	
	for (interline = first_interline;
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

	    /*
	    canvas.drawRect(accumulated_width,
			    accumulated_height,
			    accumulated_width+line.width(),
			    accumulated_height+interline.height,
			    GRASP.paint);
	    */
	    
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
		
		float dh = (line_height - h);

		if (h > maximum_height)  {
		    maximum_height = h;
		}
		
		canvas.save();
		canvas.translate(accumulated_width,
				 accumulated_height);
		canvas.clipRect(0,0,w,h+60);
		bit.render(canvas);
		canvas.restore();
		
		accumulated_width += w;
	    }
	    if (accumulated_width > maximum_width) {
		maximum_width = accumulated_width;
	    }
	    
	    accumulated_height += maximum_height;
	}

	

	Paren.Left.render(canvas, GRASP.paint,
			  accumulated_height - 60);
	canvas.save();
	canvas.translate(maximum_width, 0);

	Paren.Right.render(canvas, GRASP.paint,
			   accumulated_height - 60);
	
	canvas.restore();
	GRASP.paint.setColor(previous_color);
    }

    
    @Override public float width() {
	return 2*parenWidth+first_interline.maximum_width();
    }
    
    @Override public float height() {
	return first_interline.onward_height();
    }
}
