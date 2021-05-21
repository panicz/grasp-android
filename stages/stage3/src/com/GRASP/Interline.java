package com.GRASP;

class Interline implements Highlightable {
    public float height;
    /*@Nullable*/ public Line following_line = null;
    
    public float onward_height() {
	return height
	    + ((following_line == null)
	       ? 0
	       : following_line.onward_height());
    }
    
    public float maximum_width() {
	if (following_line == null) {
	    return 0;
	}
	float line_width = following_line.width();
	float remaining_width =
	    (following_line.next_interline == null)
	    ? 0
	    : following_line.next_interline.maximum_width();
	if (line_width > remaining_width) {
	    return line_width;
	}
	return remaining_width;
    }

    public Interline(int lines, Line line) {
	height = 16*lines;
	following_line = line;
    }

    public Interline(int lines) {
	height = 16*lines;
    }

    public Interline remove_following_line() {

	if (following_line != null) {
	    height += following_line.height();
	    Interline next_interline = following_line.next_interline;
	    if (next_interline != null) {
		height += next_interline.height;
		following_line = next_interline.following_line;
	    }
	    else {
		following_line = null;
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
