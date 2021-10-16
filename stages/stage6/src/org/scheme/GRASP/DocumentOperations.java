package org.scheme.GRASP;


interface DocumentOperations {
    //boolean moveNode();

    Track track(float x, float y);

    Bit take(Track track);
    Bit copy(Track track);
    Indexable refer(Track track);
    
    void insert(Bit bit, Track track);

    //void spliceBox(Track track);
    //Bit createBox(float x, float y, float w, float h);
    //void resize(Track track, float w, float h);
}
