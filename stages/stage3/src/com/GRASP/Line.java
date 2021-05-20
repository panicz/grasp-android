package com.GRASP;
import java.lang.Math;


class Line {
    /*@Nullable*/ public Space first_space = null;
    /*@Nullable*/ public Interline next_interline = null;
    
    public float onward_height() {
	return height()
	    + ((next_interline == null)
	       ? 0 : next_interline.onward_height());
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

    public Line(Space space) {
	first_space = space;
    }
    
    public Line appendLine(int lines, Space space) {
	next_interline = new Interline(lines,
				       new Line(space));

	return next_interline.following_line;
    }
}
