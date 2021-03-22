package com.GRASP;

import android.graphics.Canvas;
import android.view.KeyEvent;


class IdleBox implements Box {

    public static class ShowKeyboard
	implements TouchHandler {
	@Override
	public ActionResult action(float x, float y) {
	    if (GRASP.desktop != null) {
		GRASP.desktop.showKeyboard();
	    }
	    return ActionProcess;
	}
    }

    public static TouchHandler showKeyboard
	= new ShowKeyboard();

    
    @Override
    public ActionResult onSingleTap(float x, float y) {
	return ActionIgnore;
    }

    @Override
    public ActionResult onDoubleTap(float x, float y) {
	return ActionIgnore;
    }

    @Override
    public ActionResult onHold(float x, float y) {
	return ActionIgnore;
    }

    @Override
    public ActionResult onPress(float x, float y,
				int finger) {
	return ActionIgnore;
    }

    @Override
    public ActionResult onUnpress(float x, float y,
				  int finger) {
	return ActionIgnore;
    }
    
    @Override
    public ActionResult onRelease(float x, float y,
				  int finger) {
	return ActionIgnore;
    }

    @Override
    public ActionResult onKeyUp(KeyEvent event) {
	return ActionIgnore;
    }

    @Override
    public ActionResult onKeyDown(KeyEvent event) {
	return ActionIgnore;
    }

    @Override
    public ActionResult onMotion(float [] x, float [] y,
				 boolean [] finger,
				 int max_finger) {
	return ActionIgnore;
    }

    @Override
    public void draw(Canvas canvas) {}

    @Override
    public boolean contains(float x, float y) {
	return false;
    }

    @Override
    public boolean is_embeddable() {
	return false;
    }
    
    @Override
    public boolean accepts(Box b, float x, float y) {
	return false;
    }

    @Override
    public ActionResult onDragOver(Box b,
				   float x, float y) {
	return ActionIgnore;
    }

    @Override
    public ActionResult next() {
	return ActionIgnore;
    }

    @Override
    public ActionResult prev() {
	return ActionIgnore;
    }

    @Override
    public boolean precedes(Box b) {
	return false;
    }
    
    @Override
    public ActionResult addChild(Box c, float x, float y){
	return ActionIgnore;
    }
    
    @Override
    public float getWidth() {
	return 0;
    }

    @Override
    public float getHeight() {
	return 0;
    }

    @Override
    public void moveBy(float dx, float dy) {}

}
