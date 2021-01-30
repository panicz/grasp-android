package com.GRASP;
import android.graphics.RectF;


abstract class Split extends Interactions {
    protected static final float bar_width = 20.0f;

    public Interactions firstPanel;
    public Interactions secondPanel;

    public Split(float x, float y, float w, float h,
		 Interactions first, Interactions second) {
	super(x, y, w, h);
	firstPanel = first;
	secondPanel = second;    }

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
    public Interactions splitHorizontallyBy(RectF line) {
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
    public Interactions splitVerticallyBy(RectF line) {
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

    protected float grabbed_x;
    protected float grabbed_y;
    protected float closing_threshold = 5000;
    
    public void startResizing(float x, float y) {
	grabbed_x = x;
	grabbed_y = y;
    }

    public abstract void resizeBy(float dx, float dy);
    
    public void resizeTo(float x, float y) {
	resizeBy(x-grabbed_x, y-grabbed_y);
	grabbed_x = x;
	grabbed_y = y;
    }
    
}
