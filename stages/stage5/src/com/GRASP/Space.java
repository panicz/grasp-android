package com.GRASP;
import android.graphics.Paint;
import java.lang.Math;


class Space implements Highlightable {
    public float width;
    public Bit following_bit = null;

    static Paint paint = null;
    public static final float min_width = 8.0f;
    
    public float maximum_height() {
	if (following_bit == null) {
	    return 0;
	}
	float element_height = following_bit.height();
	float remaining_height =
	    (following_bit.following_space() == null
	     ? 0
	     : following_bit.following_space().maximum_height());
	if (element_height > remaining_height) {
	    return element_height;
	}
	return remaining_height;
    }

    public float minimum_height() {
	if (following_bit == null) {
	    return 0;
	}
	float element_height = following_bit.min_height();
	float remaining_height =
	    (following_bit.following_space() == null
	     ? 0
	     : following_bit.following_space().minimum_height());
	if (element_height > remaining_height) {
	    return element_height;
	}
	return remaining_height;
    }
    
    public float onward_width() {
	return width
	    + ((following_bit == null)
	       ? 0
	       : (following_bit.width()
		  + ((following_bit.following_space() == null)
		     ? 0
		     : following_bit.following_space().onward_width()
		     )));
    }
    
    public float minimum_width() {
	return Math.min(min_width, width)
	    + ((following_bit == null)
	       ? 0
	       : (following_bit.min_width()
		  + ((following_bit.following_space() == null)
		     ? 0
		     : (following_bit.following_space()
			.minimum_width())
		     )));
    }
    
    public Space(float w, Bit bit) {
	width = Math.max(min_width, w);
	following_bit = bit;
    }

    public Space(float w) {
	this(w, (Bit) null);
    }


    public Space deep_copy() {
	return new Space(width, ((following_bit == null)
				 ? null
				 : following_bit.deep_copy()));
    }
    
    public boolean insertAt(float x, float y, DragAround bit) {
	
	if(bit.x < width || following_bit == null) {
 	    width = Math.max(min_width, bit.x);
	}
	Space nextSpace = new Space(width-bit.x-bit.width(),
				    following_bit);
	following_bit = bit.target;
	following_bit.set_following_space(nextSpace);
	return true;
    }
    
    public Bit remove_following_bit() {
	Bit removed = following_bit;
	
	if (following_bit != null) {
	    width += following_bit.width();
	    
	    Space following_space = following_bit.following_space();
	    following_bit.set_following_space(null);
	    if (following_space != null) {
		width += following_space.width;
		following_bit = following_space.following_bit;
	    }
	    else {
		following_bit = null;
	    }
	}

	return removed;
    }

    private float highlighted = Float.NaN;
    
    @Override
    public boolean is_highlighted() {
	return !Float.isNaN(highlighted);
    }

    @Override
    public void highlight(float x, float y) {
	highlighted = x;
    }

    @Override
    public void unhighlight() {
	highlighted = Float.NaN;
    }



    
}
