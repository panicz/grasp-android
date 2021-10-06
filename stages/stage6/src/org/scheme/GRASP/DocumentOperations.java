package org.scheme.GRASP;

import java.util.List;
import java.util.ArrayList;


interface DocumentOperations {
    //boolean moveNode();
    class Track {
	// even -> spaces between bits
	// odd -> bits
	public List<Integer> turn = new ArrayList<Integer>();
	public Object last = null;
    }

    Track track(float x, float y);

    Bit take(Track track);
    Bit copy(Track track);
    Bit refer(Track track);
    
    void insert(Bit bit, Track track);
}
