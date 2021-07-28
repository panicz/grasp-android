package com.GRASP;

import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Typeface;
import java.lang.StringBuilder;
import java.lang.Math;

class TextInput implements Pad {
    StringBuilder contents;
    int cursor_position = 0;
    int selection_start = 0;
    float font_size;
    Typeface font;
    float min_width;
    static Paint paint = null;

    public TextInput(float min_width, String initial_text, float font_size, Typeface font) {
	this.min_width = min_width;
	contents = new StringBuilder(initial_text);
	this.font_size = font_size;
	this.font = font;
	cursor_position = selection_start = initial_text.length();
	if (paint == null) {
	    paint = new Paint();
	    paint.setColor(0xffffffff);
	    paint.setFlags(Paint.ANTI_ALIAS_FLAG);
	}
    }

    public TextInput(float min_width) {
	this(min_width, "", 76, GRASP.strings_font);
    }
    
    @Override
    public void render(Canvas canvas) {
	int left = Math.min(cursor_position, selection_start);
	int right = Math.max(cursor_position, selection_start);
	String before_selection = contents.substring(0, left);
	String selection = contents.substring(left, right);
	String after_selection = contents.substring(right);
	GRASP.paint.setTypeface(font);
	GRASP.paint.setTextSize(font_size);
	
	canvas.drawRect(0, 0, width(), height(), paint);
	
	canvas.drawText(before_selection, 0.0f, font_size,
			GRASP.paint);
	float prefix_width = GRASP.paint
	    .measureText(before_selection);
	float selection_width = Math.max(2.0f, GRASP.paint
					 .measureText(selection));
	canvas.drawRect(prefix_width, 0,
			prefix_width+selection_width, font_size,
			GRASP.paint);
	canvas.drawText(selection, prefix_width, font_size, paint);
	canvas.drawText(after_selection,
			prefix_width+selection_width,
			font_size, GRASP.paint);
    }

    @Override
    public float width() {
	paint.setTypeface(font);
	paint.setTextSize(font_size);
	return Math.max(min_width, paint.measureText(contents.toString()));
    }
    
    @Override
    public float height() {
	return font_size;
    }

    @Override
    public void trySetSize(float x, float y) {
	
    }

    @Override
    public Drag onPress(Screen screen, byte finger, float x, float y) {
	return null;
    }

    @Override
    public void onClick(Screen screen, byte finger, float x, float y) {

    }

    @Override
    public Drag onSecondPress(Screen screen, byte finger, float x, float y) {
	return null;
    }

    @Override
    public void onDoubleClick(Screen screen, byte finger, float x, float y) {

    }

    @Override
    public Drag onHold(Screen screen, byte finger, float x, float y) {
	return null;
    }

    // these are only triggered when the parent decides so
    // (currently only triggered from Popup and Below,
    // and handled by Below and Button)
    @Override
    public void onDragOver(Screen screen, byte finger, float x, float y) {
    }

    @Override
    public void onDragOut(Screen screen, byte finger) {
    }

    @Override
    public void onRelease(Screen screen, byte finger, float x, float y) {
    }

}
