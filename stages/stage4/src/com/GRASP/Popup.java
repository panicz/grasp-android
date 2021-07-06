package com.GRASP;

import android.graphics.Canvas;

class Popup implements Pad {

    static final Shift shift = new Shift();
    
    protected static Drag translate(Drag drag, float x, float y) {
	if (drag == null) {
	    return null;
	}
		
	shift.set(x, y);
	return drag.outwards(shift);
    }
    
    static final float margin_width = 4;
    static final float bar_height = 4;
    
    float left, top;
    /*
    class MoveWindow implements Drag {

    }

    class ResizeWindow implements Drag {

    }*/
    
    Pad content;

    public Popup(Pad target, float x, float y) {
	content = target;
	left = x;
	top = y;
    }
    
    @Override
    public void render(Canvas canvas) {
	// render round rectangle
	GRASP.paint.setColor(0x77000000);
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
	float w = content.width();
	float h = content.height();
	
	if (left+margin_width < x
	    && x < left+margin_width+w
	    && top+bar_height < y
	    && y < top+bar_height+h) {
	    return translate(content.onPress(screen,finger,
					     x-left-margin_width,
					     y-top-bar_height),
			     left+margin_width,
			     top+bar_height);
	}
	return null;
    }

    @Override
    public void onClick(Screen screen,
			byte finger,
			float x, float y) {
	if (left+margin_width < x
	    && x < left+margin_width+content.width()
	    && top+bar_height < y
	    && y < top+bar_height+content.height()) {
	    content.onClick(screen,finger,
			    x-left-margin_width,
			    y-top-bar_height);
	}
	else {
	    screen.layers.removeLastOccurrence(this);
	}
    }

    @Override
    public Drag onSecondPress(Screen screen,
			      byte finger,
			      float x, float y) {
	if (left+margin_width < x
	    && x < left+margin_width+content.width()
	    && top+bar_height < y
	    && y < top+bar_height+content.height()) {
	    return translate(content
			     .onSecondPress(screen,finger,
					    x-left-margin_width,
					    y-top-bar_height),
			     left+margin_width,
			     top+bar_height);
	}
	return null;
    }

    @Override
    public void onDoubleClick(Screen screen,
			      byte finger,
			      float x, float y) {
	if (left+margin_width < x
	    && x < left+margin_width+content.width()
	    && top+bar_height < y
	    && y < top+bar_height+content.height()) {
	    content.onDoubleClick(screen,finger,
				  x-left-margin_width,
				  y-top-bar_height);
	}	
    }

    @Override
    public Drag onHold(Screen screen,
		       byte finger,
		       float x, float y) {
	if (left+margin_width < x
	    && x < left+margin_width+content.width()
	    && top+bar_height < y
	    && y < top+bar_height+content.height()) {
	    return translate(content.onHold(screen,finger,
					    x-left-margin_width,
					    y-top-bar_height),
			     left+margin_width,
			     top+bar_height);
	}	
	return null;
    }
}
