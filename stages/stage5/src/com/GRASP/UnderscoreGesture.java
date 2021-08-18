package com.GRASP;

class UnderscoreGesture extends Gesture {

    float epsilon;
    public UnderscoreGesture(float eps) {
	name = "underscore";
	epsilon = eps;

    }

    Point a = new Point();
    Point b = new Point();
    Point c = new Point();
    Point d = new Point();

    float top, left, bottom, right;
    Editor editor;
    
    @Override
    public boolean recognizes(Shape shape,
			      Screen screen) {
	return false;
    }

    @Override
    public void perform(Shape shape, Screen screen) {

    }

    
}
