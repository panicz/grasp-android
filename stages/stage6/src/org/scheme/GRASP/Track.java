package org.scheme.GRASP;

import java.util.List;
import java.util.ArrayList;

class Track {
    // even -> spaces between bits
    // odd -> bits
    public List<Integer> turns = new ArrayList<Integer>();
    //public Object last = null;
    float sx = 0, sy = 0, dx = 0, dy = 0;
    
    @Override
    public String toString() {
	return turns.toString()
	    +"("+(int)sx+", "+(int)sy+", "
	    +(int)dx+", "+(int)dy+")";
    }
}
