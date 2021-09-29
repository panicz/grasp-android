package org.scheme.GRASP;

class HorizontalLineAcrossTheScreen extends Gesture {
    public HorizontalLineAcrossTheScreen() {
	name = "horizontal-line-across-the-screen";
    }

    @Override
    public boolean recognizes(Shape shape, Screen screen) {
	return shape.strokes.size() == 1
	    && shape.getHeight() < 120
	    && screen.panel.canBeSplittedVerticallyBy(shape.area);
    }

    @Override
    public void perform(Shape shape, Screen screen) {
	screen.panel = screen.panel.splitVerticallyBy(shape.area);
    }

    public static Gesture instance = new HorizontalLineAcrossTheScreen();
}
