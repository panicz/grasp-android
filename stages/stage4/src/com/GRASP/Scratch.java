package com.GRASP;

class Scratch extends Document {
    private Scratch() {
	super();
	path = "<scratch>";
    }

    private static Scratch _instance = null;
    
    public static Scratch instance() {
	if (_instance == null) {
	    _instance = new Scratch();
	}
	return _instance;
    }
}

