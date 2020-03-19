package com.GRASP;

import android.graphics.Canvas;

class IdleBox implements Box {
    
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
    public ActionResult onKeyUp(int code) {
	return ActionIgnore;
    }

    @Override
    public ActionResult onKeyDown(int code) {
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
    public boolean accepts(Box b, float x, float y) {
	return false;
    }

    @Override
    public void addChild(Box c, float x, float y) {}
    
    @Override
    public float getWidth() {
	return 0;
    }

    @Override
    public float getHeight() {
	return 0;
    }
}
