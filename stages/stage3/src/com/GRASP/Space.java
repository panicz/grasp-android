package com.GRASP;
import android.graphics.Paint;


class Space implements Highlightable {
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

    
    public Space(float w, Bit bit) {
	width = w;
	following_bit = bit;
    }

    public Space(int columns) {
	this(columns, (Bit) null);
    }

    public boolean insertAt(float x, float y, Bit bit) {
	assert(x >= 0);
	assert(x <= width);
	Space nextSpace = new Space(width-x, following_bit);
	following_bit = bit;
	width = x;
	bit.following_space = nextSpace;
	return true;
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
