package com.GRASP;

import android.graphics.Canvas;


class Box extends Bit {
    /*@NonNull*/ public Interline first_interline = null;

    @Override
    public void render(Canvas canvas) {

	float accumulated_height = 0;

	Interline interline;
	
	for (interline = first_interline;
	     interline != null
		 && interline.following_line != null;
	     interline = interline.following_line.next_interline) {
	    Line line = interline.following_line;

	    accumulated_height += interline.height;
	    
	    float accumulated_width = 0;
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

		if (h > maximum_height)  {
		    maximum_height = h;
		}
		
		canvas.save();
		canvas.translate(accumulated_width,
				 accumulated_height);
		canvas.clipRect(0, 0, w, h);
		bit.render(canvas);
		canvas.restore();
		
		accumulated_width += w;
	    }

	    accumulated_height += maximum_height;
	}
    }

    
    @Override public float width() {
	return first_interline.maximum_width();
    }
    
    @Override public float height() {
	return first_interline.onward_height();
    }
}
