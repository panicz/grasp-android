package org.scheme.GRASP;
import android.graphics.Paint;
import java.lang.Math;


class Space implements Indexable {
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
	     : following_bit
	     .following_space()
	     .maximum_height());
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
	     : following_bit.following_space()
	     .minimum_height());
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
		  + ((following_bit.following_space()
		      == null)
		     ? 0
		     : following_bit.following_space()
		     .onward_width()
		     )));
    }
    
    public float minimum_width() {
	return Math.min(min_width, width)
	    + ((following_bit == null)
	       ? 0
	       : (following_bit.min_width()
		  + ((following_bit.following_space()
		      == null)
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

    public Space() {
	this(min_width, (Bit) null);
    }

    
    public Space deep_copy() {
	return new Space(width,
			 ((following_bit == null)
			  ? null
			  : following_bit.deep_copy()));
    }
    
    public boolean insertAt(float x, float y,
			    DragAround bit) {

	Space nextSpace = new Space(width
				    -bit.x
				    -bit.width(),
				    following_bit);
       
	if(bit.x < width || following_bit == null) {
 	    width = Math.max(min_width, bit.x);
	}
	following_bit = bit.target;
	following_bit.set_following_space(nextSpace);
	return true;
    }
    
    public Bit remove_following_bit(Line line) {
	Bit removed = following_bit;
	float initial_height = line.height();
	if (following_bit != null) {
	    float dw = following_bit.width();
	    width += dw;
	    Space following_space =
		following_bit.following_space();
	    following_bit.set_following_space(null);
	    if (following_space != null) {
		width += following_space.width;
		following_bit =
		    following_space.following_bit;
	    }
	    else {
		following_bit = null;
	    }
	}

	float final_height = line.height();

	float decrease = initial_height - final_height;

	assert(decrease >= 0);
	
	if (line.next_interline == null) {
	    line.next_interline =
		new Interline(decrease);
	}
	else {
	    line.next_interline.height += decrease;
	}

	return removed;
    }
    
}
