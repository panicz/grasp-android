package com.GRASP;

class Box extends Bit {
    /*@NonNull*/ public Interline first_interline = null;
    
    @Override public float width() {
	return first_interline.maximum_width();
    }
    
    @Override public float height() {
	return first_interline.onward_height();
    }
}
