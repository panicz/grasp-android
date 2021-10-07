package org.scheme.GRASP;


interface DocumentOperations {
    //boolean moveNode();

    Track track(float x, float y);

    Bit take(Track track);
    Bit copy(Track track);
    Bit refer(Track track);
    
    void insert(Bit bit, Track track);
}
