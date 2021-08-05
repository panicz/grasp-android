package com.GRASP;

class VerticalLineAcrossTheScreen extends Gesture {
    public VerticalLineAcrossTheScreen() {
	name = "vertical-line-across-the-screen";
    }

    @Override
    public boolean recognize(Shape shape, Screen screen) {
	return shape.strokes.size() == 1
	    && shape.getWidth() < 120
	    && screen.panel.canBeSplittedHorizontallyBy(shape.area);
    }

    @Override
    public boolean perform(Shape shape, Screen screen) {
	screen.panel = screen.panel.splitHorizontallyBy(shape.area);
	return true;
    }

    public static Gesture instance = new VerticalLineAcrossTheScreen();

}


    
