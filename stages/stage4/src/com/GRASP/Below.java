package com.GRASP;

import android.graphics.Canvas;

class Below implements Pad, Drag {
    Pad [] contents;
    Pad hovered = null;
    int focus = 0;
    
    static final Shift shift = new Shift();

    public Below(Pad ... items) {
	contents = items;
	float w = width();
	
	for (int i = 0; i < contents.length; ++i) {
	    contents[i].trySetSize(w, contents[i].height());
	}
    }
    
    protected static Drag translate(Drag drag, float x, float y) {
	if (drag == null) {
	    return null;
	}
		
	shift.set(x, y);
	return drag.outwards(shift);
    }
    
    @Override
    public void render(Canvas canvas) {
	canvas.save();
	boolean something_was_dislayed = false;
	for (int i = 0; i < contents.length; ++i) {
	    float w = contents[i].width();
	    float h = contents[i].height();
	    if (canvas.quickReject(0.0f, 0.0f, w, h,
				   Canvas.EdgeType.BW)) {
		if (something_was_dislayed) {
		    break;
		}
	    }
	    else {
		contents[i].render(canvas);
		something_was_dislayed = true;
	    }
	    canvas.translate(0, h);
	}
	canvas.restore();
    }

    @Override
    public float width() {
	float max_width = 0;
	for (int i = 0; i < contents.length; ++i) {
	    float w = contents[i].width();
	    if (w > max_width) {
		max_width = w;
	    }
	}
	return max_width;
    }

    @Override
    public float height() {
	float total_height = 0;
	for (int i = 0; i < contents.length; ++i) {
	    total_height += contents[i].height();
	}
	return total_height;
    }

    @Override
    public void trySetSize(float x, float y) {

    }


    float total_height;
    Pad itemAt(float x, float y) {
	if (x < 0) {
	    return null;
	}
	
	total_height = 0;
	for (int i = 0; i < contents.length; ++i) {
	    float h = contents[i].height();
	    if (total_height <= y && y < total_height + h) {
		if (x < contents[i].width()) {
		    return contents[i];
		}
		else {
		    return null;
		}
	    }
	    
	    total_height += h;
	}
	return null;
    }
    
    @Override
    public Drag onPress(Screen screen,
			byte finger,
			float x, float y) {

	Pad target = itemAt(x, y);

	if (target == null) {
	    return this;
	}

	Drag drag = target.onPress(screen, finger,
				   x, y-total_height);
	if (drag == null) {
	    return this;
	}

	return translate(drag, 0, total_height);
    }

    @Override
    public void onClick(Screen screen,
			byte finger,
			float x, float y) {
	Pad target = itemAt(x, y);

	if (target != null) {
	    target.onClick(screen,
			   finger,
			   x, y-total_height);
	}
    }

    @Override
    public Drag onSecondPress(Screen screen,
			      byte finger,
			      float x, float y) {
	Pad target = itemAt(x, y);

	if (target == null) {
	    return null;
	}

	return translate(target.onSecondPress(screen,
					      finger,
					      x, y-total_height),
			 0, total_height);
    }

    @Override
    public void onDoubleClick(Screen screen,
			      byte finger,
			      float x, float y) {

	Pad target = itemAt(x, y);

	if (target != null) {
	    target.onDoubleClick(screen,
				 finger,
				 x, y-total_height);
	}

    }

    @Override
    public Drag onHold(Screen screen,
		       byte finger,
		       float x, float y) {
	Pad target = itemAt(x, y);

	if (target == null) {
	    return this;
	}

	return translate(target.onHold(screen,
				       finger,
				       x, y-total_height),
			 0, total_height);
    }

    @Override
    public boolean onKeyUp(Screen screen, int keycode,
			   char unicode, int meta) {
	if (0 <= focus && focus < contents.length) {
	    return contents[focus].onKeyUp(screen, keycode, unicode, meta);
	}
	return false;
    }

    @Override
    public boolean onKeyDown(Screen screen, int keycode,
			     char unicode, int meta) {
	if (0 <= focus && focus < contents.length) {
	    return contents[focus].onKeyDown(screen, keycode, unicode, meta);
	}
	return false;
    }
    
    float dx = 0;
    float dy = 0;
    @Override
    public Drag outwards(Transform transform) {
	float x = transform.unx(dx, dy);
	float y = transform.uny(dx, dy);
	dx = x;
	dy = y;
	return this;
    }
    
    @Override
    public Drag inwards(Transform transform) {
	float x = transform.x(dx, dy);
	float y = transform.y(dx, dy);
	dx = x;
	dy = y;
	return this;
    }


    @Override
    public void onDragOver(Screen screen, byte finger,
			   float x, float y) {
	x += dx;
	y += dy;
	assert(hovered == null);
	hovered = itemAt(x, y);
	if (hovered != null) {
	    hovered.onDragOver(screen, finger, x, y);
	}	
    }

    @Override
    public void onDragOut(Screen screen, byte finger) {
	if (hovered != null) {
	    hovered.onDragOut(screen, finger);
	}
	hovered = null;
    }

    @Override
    public void onRelease(Screen screen, byte finger,
			  float x, float y) {
	x += dx;
	y += dy;

	if (hovered != null) {
	    hovered.onRelease(screen, finger, x, y);
	}
	hovered = null;	
    }

    @Override
    public void move(Screen screen, float x, float y,
		     float _dx, float _dy) {
	x += dx;
	y += dy;

	Pad item = itemAt(x, y);
	if (item != hovered) {
	    if (hovered != null) {
		hovered.onDragOut(screen, (byte)0);
	    }
	    hovered = item;
	    if (hovered != null) {
		hovered.onDragOver(screen, (byte)0, x, y);
	    }
	}
    }
    
    @Override
    public void drop(Screen screen, float x, float y,
		     float vx, float vy) {
	x += dx;
	y += dy;

	if (hovered != null) {
	    hovered.onRelease(screen, (byte)0, x, y);
	}
	hovered = null;

	dx = dy = 0;
    }
    

    
}
