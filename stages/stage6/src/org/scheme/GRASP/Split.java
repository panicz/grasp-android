package org.scheme.GRASP;
import android.graphics.RectF;

abstract class Split extends Panel implements Drag {
    public static final float bar_width = 20.0f;
    public static final float closing_threshold = 5000.0f;
    
    public Panel firstPanel;
    public Panel secondPanel;
    public Panel keyboardFocus;

    public Split(float x, float y, float w, float h,
		 Panel first, Panel second, Panel focus) {
	super(x, y, w, h);
	firstPanel = first;
	secondPanel = second;
	assert(focus == first || focus == second);
	keyboardFocus = focus;
    }

    @Override
    public boolean closeDocument(Document document) {
	return firstPanel.closeDocument(document)
	    && secondPanel.closeDocument(document);
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
    public void onDelete() {
	firstPanel.onDelete();
	secondPanel.onDelete();
    }
    
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
    public Drag stretchFrom(byte finger, float x, float y) {
	return null;
    }
    
    @Override
    public void stretch() {
	firstPanel.stretch();
	secondPanel.stretch();
    }

    @Override
    public Drag inwards(Transform transform) {
	return this;
    }

    @Override
    public Drag outwards(Transform transform) {
	return this;
    }
    
    @Override
    public Drag onPress(Screen screen,
			byte finger,
			float x, float y) {
	return this;
    }

    @Override    
    public void onClick(Screen screen,
			byte finger,
			float x, float y) {}

    @Override
    public Drag onSecondPress(Screen screen,
			      byte finger,
			      float x, float y) {
	return null;
    }

    @Override
    public void onDoubleClick(Screen screen,
			      byte finger,
			      float x, float y) {
    }

    @Override
    public Drag onHold(Screen screen,
		       byte finger,
		       float x, float y) {
	return null;
    }

    @Override
    public boolean onKeyUp(Screen screen, int keycode,
			   char unicode, int meta) {
	return keyboardFocus.onKeyUp(screen, keycode, unicode, meta);
    }

    @Override
    public boolean onKeyDown(Screen screen, int keycode,
			     char unicode, int meta) {
	return keyboardFocus.onKeyDown(screen, keycode, unicode, meta);
    }
}
