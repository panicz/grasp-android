package com.GRASP;
import android.view.View;

import android.content.res.AssetManager;

class Utils {
    public static float max(float ... args) {
	float result = Float.NEGATIVE_INFINITY;
	for (int i = 0; i < args.length; ++i) {
	    if (args[i] > result) {
		result = args[i];
	    }
	}
	return result;
    }

    public static float min(float ... args) {
	float result = Float.POSITIVE_INFINITY;
	for (int i = 0; i < args.length; ++i) {
	    if (args[i] < result) {
		result = args[i];
	    }
	}
	return result;
    }

    public static Logger logger = null;
    public static View view = null;
    public static AssetManager assets = null;
    
    public static void log(String s) {
	if (logger != null) {
	    logger.log(s);
	}
	
	if (view != null) {
	    //view.invalidate();
	}
	
    }

    
}
