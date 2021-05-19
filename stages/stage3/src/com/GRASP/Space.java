package com.GRASP;
import android.graphics.Paint;


class Space {
    public float width;
    /*@Nullable*/ public Bit following_bit = null;

    static Paint paint = null;

    
    public float maximum_height() {
	if (following_bit == null) {
	    return 0;
	}
	float element_height = following_bit.height();
	float remaining_height =
	    (following_bit.following_space == null
	     ? 0
	     : following_bit.following_space.maximum_height());
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
		  + ((following_bit.following_space == null)
		     ? 0
		     : following_bit.following_space.onward_width()
		     )));
    }

    
    public Space(int columns, Bit bit) {
	if (paint == null) {
	    paint = new Paint();
	    paint.setTypeface(GRASP.symbols_font);
	    paint.setTextSize(Atom.text_size);
	}

	width = paint.measureText("-")*columns;
	following_bit = bit;
    }

    public Space(int columns) {
	this(columns, (Bit) null);
    }

    public Space remove_following_bit() {
	if (following_bit != null) {
	    width += following_bit.width();
	    Space following_space = following_bit.following_space;
	    if (following_space != null) {
		width += following_space.width;
		following_bit = following_space.following_bit;
	    }
	    else {
		following_bit = null;
	    }
	}
	return this;
    }
    
}
