package com.GRASP;

//import java.lang.Math;

class S {
    public static int count(char c, String s) {
	if (s == null) {
	    return 0;
	}
	
	int result = 0;
	for (int i = 0; i < s.length(); ++i) {
	    if(s.charAt(i) == c) {
		++result;
	    }
	}
	return result;
    }
    
    public static int lines(String s) {
	return S.count('\n', s);//+1;
    }

    public static int ultimate_line_width(String s) {
	if (s == null) {
	    return 0;
	}
	int start = s.lastIndexOf('\n');
	if (start == -1) {
	    start = 0;
	}
	return s.length() - start;
    }

    public static int first_line_width(String s) {
	if (s == null) {
	    return 0;
	}

	for (int i = 0; i < s.length(); ++i) {
	    if(s.charAt(i) == '\n') {
		return i;
	    }
	}
	return s.length();
    }

    public static float qr(float x) {
	return x*x;
    }

    public static int ign(float x) {
	if (x < 0) {
	    return -1;
	}
	if (x > 0) {
	    return +1;
	}
	return 0;
    }
    
}
