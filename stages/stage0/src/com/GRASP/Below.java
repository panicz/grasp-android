package com.GRASP;

import android.graphics.Canvas;


class Below extends Widget {
    Widget widgets[];

    public Below(Widget ... widgets) {
	this.widgets = widgets;
    }

    @Override
    public void render(Canvas canvas,
		       float clip_left, float clip_top,
		       float clip_width, float clip_height) {
	float top = 0;
	log("stacked elements: "+widgets.length);
	for (int i = 0; i < widgets.length; ++i) {
	    Widget wi = widgets[i];
	    float w = wi.width();
	    float h = wi.height();
	    if (top + h > clip_top) {
		log("rendering "+i);
		canvas.save();
		canvas.translate(0, top);
		canvas.clipRect(0, 0, w, h);
		wi.render(canvas,
			  clip_left,
			  max(0, clip_top - top),
			  clip_width,
			  max(0, clip_height - top));
		canvas.restore();
	    }
	    else {
		log("skipping "+i);
	    }
	    top += h;
	    if (top > clip_height) {
		// optymalizacja: wyszlismy poza obszar
		// przyciecia, wiec dalsze rysowanie
		// nie ma sensu
		log("giving up on "+i);
		return;
	    }
	}
    }
    //Skim skim(float x, float y);

    @Override
    public float width() {
	float max = 0;
	for (int i = 0; i < widgets.length; ++i) {
	    float w = widgets[i].width();
	    if (w > max) {
		max = w;
	    }
	}
	return max;
    }

    @Override
    public float height() {
	float total = 0;
	for (int i = 0; i < widgets.length; ++i) {
	    total += widgets[i].height();
	}
	return total;
    }
}
