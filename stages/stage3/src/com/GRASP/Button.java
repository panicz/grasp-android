package com.GRASP;

//import android.graphics.Canvas;
//import android.graphics.Color;

class Button {
    public static final float height = 80.0f;
    public String label;
    public Action action;
    public Button(String label, Action action) {
	this.label = label;
	this.action = action;
    }
}
