package org.scheme.GRASP;

import java.util.List;
import java.util.ArrayList;

class Track {
    // even -> spaces between bits
    // odd -> bits
    public List<Integer> turns = new ArrayList<Integer>();
    //public Object last = null;
    float sx = 0, sy = 0, dx = 0, dy = 0;


    Track bit() {
	int size = turns.size();

	/*
	GRASP.log("target "+target+", x "+(int)x+", y "+(int)y
		  +", lx "+(int)lx+", ly "+(int)ly);
	*/
	if (size == 0) {
	    //GRASP.log("grabbed document");
	    return null;
	}
	
	int last = turns.get(size-1);

	if (last % 2 == 0) {
	    turns.remove(size-1);

	    size = turns.size();
	
	    if (size == 0) {
		//GRASP.log("grabbed docspace");
		return null;
	    }
	}

	return this;
    }
    
    @Override
    public String toString() {
	return turns.toString()
	    +"("+(int)sx+", "+(int)sy+", "
	    +(int)dx+", "+(int)dy+")";
    }
}
