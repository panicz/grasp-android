package com.GRASP;
import java.lang.Math;


class Line implements Location {
    public Space first_space = null;
    public Interline next_interline = null;
    
    public float onward_height() {
	return height()
	    + ((next_interline == null)
	       ? 0 : next_interline.onward_height());
    }

    public float minimum_height() {
	return Math.max(Box.min_height,
			((first_space == null)
			 ? 0
			 : first_space.minimum_height()));
    }

    public float height() {
	return Math.max(Box.min_height,
			((first_space == null)
			 ? 0
			 : first_space.maximum_height()));
    }
    
    public float width() {
	return (first_space == null)
	    ? 0
	    : first_space.onward_width();
    }

    public float minimum_width() {
	return (first_space == null)
	    ? 0
	    : first_space.minimum_width();
    }
    
    public Line(Space space) {
	first_space = space;
    }
    
    public Line appendLine(int lines, Space space) {
	next_interline = new Interline(lines,
				       new Line(space));

	return next_interline.following_line;
    }

    public Line deep_copy() {
	Line copy = new Line(first_space == null
			     ? null
			     : first_space.deep_copy());
	if (next_interline != null) {
	    copy.next_interline = next_interline.deep_copy();
	}
	return copy;
    }

    public boolean isEmpty() {
	return first_space == null
	    || first_space.following_bit == null;
    }
}
