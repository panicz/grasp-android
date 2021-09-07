package com.GRASP;

class VerticalLineAcrossTheScreen extends Gesture {
    public VerticalLineAcrossTheScreen() {
	name = "vertical-line-across-the-screen";
    }

    @Override
    public boolean recognizes(Shape shape, Screen screen) {
	return shape.strokes.size() == 1
	    && shape.getWidth() < 120
	    && screen.panel.canBeSplittedHorizontallyBy(shape.area);
    }

    @Override
    public void perform(Shape shape, Screen screen) {
	screen.panel = screen.panel.splitHorizontallyBy(shape.area);
    }

    public static Gesture instance = new VerticalLineAcrossTheScreen();

}
