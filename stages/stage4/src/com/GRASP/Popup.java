package com.GRASP;

import android.graphics.Canvas;

class Popup implements Pad, Drag {

    static final Shift shift = new Shift();
    
    protected static Drag translate(Drag drag, float x, float y) {
	if (drag == null) {
	    return null;
	}
		
	shift.set(x, y);
	return drag.outwards(shift);
    }
    
    static final float margin_width = 4;
    static final float bar_height = 40;
    
    public float left, top;
    
    class MoveWindow implements Drag {

	Popup target;

	public MoveWindow(Popup window) {
	    target = window;
	}
	
	@Override
	public void move(Screen screen, float x, float y,
			 float dx, float dy) {
	    target.left += dx;
	    target.top += dy;
	}

	@Override
	public void drop(Screen screen, float x, float y,
			 float vx, float vy) {
	    if (Math.sqrt(vx*vx + vy*vy)
		>= Split.closing_threshold) {
		screen.layers.removeLastOccurrence(target);
	    }
	}

	@Override
	public Drag outwards(Transform transform) {
	    return this;
	}

	@Override
	public Drag inwards(Transform transform) {
	    return this;
	}
    }

    MoveWindow moveAround;
    
    /*
    class ResizeWindow implements Drag {

    }*/
    
    Pad content;

    public Popup(Pad target) {
	content = target;
	moveAround = new MoveWindow(this);
    }
    
    @Override
    public void render(Canvas canvas) {
	GRASP.paint.setColor(GRASP.paint.getColor()-0x77000000);
	canvas.drawRoundRect(left, top,
			     left+width(), top+height(), 25, 25,
			     GRASP.paint);
	canvas.translate(left+margin_width, top+bar_height);
	content.render(canvas);
	canvas.translate(-left-margin_width, -top-bar_height);
	GRASP.paint.setColor(GRASP.paint.getColor()+0x77000000);
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

	if (left < x && x < left + w
	    && top < y && y < top+bar_height) {
	    return moveAround;
	}
	
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

	if (left < x && x < left + w
	    && top+bar_height+h < y && y < top+2*bar_height+h) {
	    return moveAround;
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

    @Override
    public void move(Screen screen, float x, float y,
		     float dx, float dy) {

    }

    
    @Override
    public void drop(Screen screen, float x, float y,
		     float vx, float vy) {

    }

    @Override
    public Drag outwards(Transform transform) {
	float x = transform.x(left, top);
	float y = transform.y(left, top);
	left = x;
	top = y;
	return this;
    }

    @Override
    public Drag inwards(Transform transform) {
	float x = transform.unx(left, top);
	float y = transform.uny(left, top);
	left = x;
	top = y;
	return this;
    }

    @Override
    public void onDragOver(Screen screen, byte finger,
			   float x, float y) {}

    @Override
    public void onDragOut(Screen screen, byte finger) {}

    @Override
    public void onRelease(Screen screen, byte finger,
			  float x, float y) {}

    public void centerAround(float x, float y,
			     float width, float height) {
	float w = this.width();
	float h = this.height();
	left =  Math.max(0, Math.min(width-w, x - w/2));
	top = Math.max(0, Math.min(height-h, y - h/2));
    }
    
}
