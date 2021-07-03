package com.GRASP;

import android.graphics.Canvas;

class Popup implements Pad {

    float left, top;
    Pad content;

    @Override
    public void render(Canvas canvas) {
	content.render(canvas);
    }

    @Override
    public float width() {
	// irrelevant
	return GRASP.last_known_edit_instance.width;
    }

    @Override
    public float height() {
	// irrelevant
	return GRASP.last_known_edit_instance.height;
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
	// przekazujemym
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
