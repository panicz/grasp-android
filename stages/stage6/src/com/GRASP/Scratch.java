package com.GRASP;
import java.io.File;

class Scratch extends Document {
    private Scratch() {
	super();
	file = new File(GRASP.instance.getFilesDir(), "SCRATCH");
    }

    private static Scratch _instance = null;
    
    public static Scratch instance() {
	if (_instance == null) {
	    _instance = new Scratch();
	}
	return _instance;
    }
}

