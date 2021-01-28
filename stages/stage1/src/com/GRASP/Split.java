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
    public Interactions splitHorizontallyBy(RectF line) {
	if (firstPanel.canBeSplittedHorizontallyBy(line)) {
	    GRASP.log("splitting upper" +this.toString());
	    firstPanel = firstPanel
		.splitHorizontallyBy(line);
	}
	if (secondPanel.canBeSplittedHorizontallyBy(line)) {
	    GRASP.log("splitting lower"+this.toString());
	    secondPanel = secondPanel
		.splitHorizontallyBy(line);
	}
	return this;
    }

    @Override
    public Interactions splitVerticallyBy(RectF line) {
	if (firstPanel.canBeSplittedVerticallyBy(line)) {
	    GRASP.log("splitting left"+this.toString());
	    firstPanel = firstPanel
		.splitVerticallyBy(line);
	}
	if (secondPanel.canBeSplittedVerticallyBy(line)) {
	    GRASP.log("splitting right"+this.toString());
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
}
