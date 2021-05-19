package com.GRASP;
import android.graphics.RectF;

abstract class Split extends Panel implements Drag {
    protected static final float bar_width = 20.0f;
    protected static final float closing_threshold = 5000.0f;
    
    public Panel firstPanel;
    public Panel secondPanel;

    public Split(float x, float y, float w, float h,
		 Panel first, Panel second) {
	super(x, y, w, h);
	firstPanel = first;
	secondPanel = second;
    }

    @Override
    public boolean canBeSplittedVerticallyBy(RectF line) {
	return firstPanel.canBeSplittedVerticallyBy(line)
	    || secondPanel.canBeSplittedVerticallyBy(line);
    }

    @Override
    public boolean canBeSplittedHorizontallyBy(RectF line) {
	return firstPanel.canBeSplittedHorizontallyBy(line)
	    || secondPanel.canBeSplittedHorizontallyBy(line);
    }

    @Override
    public Panel splitHorizontallyBy(RectF line) {
	if (firstPanel.canBeSplittedHorizontallyBy(line)) {
	    firstPanel = firstPanel
		.splitHorizontallyBy(line);
	}
	if (secondPanel.canBeSplittedHorizontallyBy(line)) {
	    secondPanel = secondPanel
		.splitHorizontallyBy(line);
	}
	return this;
    }

    @Override
    public Panel splitVerticallyBy(RectF line) {
	if (firstPanel.canBeSplittedVerticallyBy(line)) {
	    firstPanel = firstPanel
		.splitVerticallyBy(line);
	}
	if (secondPanel.canBeSplittedVerticallyBy(line)) {
	    secondPanel = secondPanel
		.splitVerticallyBy(line);
	}
	return this;
    }
    
    @Override
    public void scrollBy(float x, float y) {
	firstPanel.scrollBy(x, y);
	secondPanel.scrollBy(x, y);
    }
    
    public abstract void resizeBy(float dx, float dy);

    @Override
    public void move(Screen screen,
		     float x, float y,
		     float dx, float dy) {
	resizeBy(dx, dy);
    }

    @Override
    public void drop(Screen screen,
		     float x, float y,
		     float vx, float vy) {
	screen.finishResizingPanels(this, vx, vy);
    }

    @Override
    public Panel at(float x, float y) {
	return this;
    }

    @Override
    public Drag stretchFrom(int finger, float x, float y) {
	return null;
    }
    
    @Override
    public void stretch() {
	firstPanel.stretch();
	secondPanel.stretch();
    }

    @Override
    public Drag translate(float x, float y) {
	return this;
    }

    protected Drag translate(Drag drag, float x, float y) {
	if (drag == null) {
	    return null;
	}
	return drag.translate(x, y);
    }

    @Override
    public Drag onPress(Screen screen,
			int finger,
			float x, float y) {
	return this;
    }

    @Override    
    public void onClick(Screen screen,
			int finger,
			float x, float y) {}

    @Override
    public Drag onSecondPress(Screen screen,
			      int finger,
			      float x, float y) {
	return null;
    }

    @Override
    public void onDoubleClick(Screen screen,
			      int finger,
			      float x, float y) {
    }

    @Override
    public Drag onHold(Screen screen,
		       int finger,
		       float x, float y) {
	return null;
    }

    
}
