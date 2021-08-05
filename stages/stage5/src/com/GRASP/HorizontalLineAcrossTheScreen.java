package com.GRASP;

class HorizontalLineAcrossTheScreen extends Gesture {
    public HorizontalLineAcrossTheScreen() {
	name = "horizontal-line-across-the-screen";
    }

    @Override
    public boolean recognize(Shape shape, Screen screen) {
	return shape.strokes.size() == 1
	    && shape.getWidth() < 120
	    && screen.panel.canBeSplittedVerticallyBy(shape.area);
    }

    @Override
    public boolean perform(Shape shape, Screen screen) {
	screen.panel = screen.panel.splitVerticallyBy(shape.area);
	return true;
    }

    public static Gesture instance = new HorizontalLineAcrossTheScreen();
}


    
