package org.scheme.GRASP;

import java.util.List;
import java.util.ArrayList;

class Track {
    // even -> spaces between bits
    // odd -> bits
    public List<Integer> turns = new ArrayList<Integer>();
    //public Object last = null;
    float x, y;
    
    @Override
    public String toString() {
	return turns.toString();
    }
}
