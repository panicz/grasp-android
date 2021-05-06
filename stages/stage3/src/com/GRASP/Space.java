package com.GRASP;

class Space {
    public float width;
    /*@Nullable*/ public Bit following_bit = null;
    
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

    
    public Space(int columns) {
	width = GRASP.paint.measureText("-")*columns;
    }

    public Space(int columns, Bit bit) {
	width = GRASP.paint.measureText("-")*columns;
	following_bit = bit;
    }

}
