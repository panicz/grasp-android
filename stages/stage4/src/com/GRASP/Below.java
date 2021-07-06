package com.GRASP;

import android.graphics.Canvas;

class Below implements Pad {
    Pad [] contents;

    static final Shift shift = new Shift();

    public Below(Pad ... items) {
	contents = items;
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
	for (int i = 0; i < contents.length; ++i) {
	    float h = contents[i].height();
	    contents[i].render(canvas);
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
    
    @Override
    public Drag onPress(Screen screen,
			byte finger,
			float x, float y) {
	if (x < 0) {
	    return null;
	}
	
	float total_height = 0;
	for (int i = 0; i < contents.length; ++i) {
	    float h = contents[i].height();
	    if (total_height <= y && y < total_height + h) {
		if (x < contents[i].width()) {
		    return translate(contents[i]
				     .onPress(screen,
					       finger,
					      x, y-total_height),
				     0, total_height);
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
    public void onClick(Screen screen,
			byte finger,
			float x, float y) {
	if (x < 0) {
	    return;
	}
	
	float total_height = 0;
	for (int i = 0; i < contents.length; ++i) {
	    float h = contents[i].height();
	    if (total_height <= y && y < total_height + h) {
		if (x < contents[i].width()) {
		    contents[i].onClick(screen,
					finger,
					x, y-total_height);
		}
		break;
	    }
	    
	    total_height += h;
	}
    }

    @Override
    public Drag onSecondPress(Screen screen,
			      byte finger,
			      float x, float y) {
	if (x < 0) {
	    return null;
	}
	
	float total_height = 0;
	for (int i = 0; i < contents.length; ++i) {
	    float h = contents[i].height();
	    if (total_height <= y && y < total_height + h) {
		if (x < contents[i].width()) {
		    return translate(contents[i]
				     .onSecondPress(screen,
						    finger,
						    x,
						    y-total_height),
				     0, total_height);
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
    public void onDoubleClick(Screen screen,
			      byte finger,
			      float x, float y) {
	if (x < 0) {
	    return;
	}
	
	float total_height = 0;
	for (int i = 0; i < contents.length; ++i) {
	    float h = contents[i].height();
	    if (total_height <= y && y < total_height + h) {
		if (x < contents[i].width()) {
		    contents[i].onDoubleClick(screen,
					      finger,
					      x, y-total_height);
		}
		break;
	    }
	    
	    total_height += h;
	}
    }

    @Override
    public Drag onHold(Screen screen,
		       byte finger,
		       float x, float y) {
	if (x < 0) {
	    return null;
	}
	
	float total_height = 0;
	for (int i = 0; i < contents.length; ++i) {
	    float h = contents[i].height();
	    if (total_height <= y && y < total_height + h) {
		if (x < contents[i].width()) {
		    return translate(contents[i]
				     .onHold(screen,
					     finger,
					     x, y-total_height),
				     0, total_height);
		}
		else {
		    return null;
		}
	    }
	    
	    total_height += h;
	}
	return null;
    }
}
