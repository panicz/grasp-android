package com.GRASP;

import android.graphics.Canvas;

class Popup implements Pad {

    static final float margin_width = 4;
    static final float bar_height = 4;
    
    float left, top;
    
    Pad content;

    @Override
    public void render(Canvas canvas) {
	// render round rectangle
	canvas.drawRoundRect(left, top,
			     left+width(), top+height(), 25, 25,
			     GRASP.paint);
	canvas.translate(left+margin_width, top+bar_height);
	content.render(canvas);
	canvas.translate(-left-margin_width, -top-bar_height);
    }

    @Override
    public float width() {
	return content.width() + 2*margin_width;
    }

    @Override
    public float height() {
	return content.height() + 2*bar_height;
    }

    @Override
    public void trySetSize(float x, float y) {
	content.trySetSize(x - 2*margin_width, y - 2*bar_height);
    }
    
    @Override
    public Drag onPress(Screen screen,
			byte finger,
			float x, float y) {
	// jezeli wcisnelismy na ramke u gory
	// to przeciagamy wyrazenie,
	// u dolu - byc moze zmieniamy jego rozmiar
	return null;
    }

    @Override
    public void onClick(Screen screen,
			byte finger,
			float x, float y) {
	
    }

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
}
