package com.GRASP;
import android.graphics.Canvas;
import android.graphics.RectF;

class VerticalSplit extends Split {

    public VerticalSplit(float x, float y, float w, float h,
			 Panel top,
			 Panel bottom) {
	super(x, y, w, h, top, bottom);
    }

    public VerticalSplit(Panel panel,
			 RectF rect) {
	super(panel.left(), panel.top(),
	      panel.width(), panel.height(),
	      panel, panel.copy());
	float center = (rect.top + rect.bottom)/2.0f;
	firstPanel.setHeight(center - top() - bar_width/2.0f);

	secondPanel.setTop(firstPanel.height() + bar_width);
	secondPanel.setHeight(height() - firstPanel.height()
			      - bar_width);

	secondPanel.scrollBy(0, -(firstPanel.height()
				  + bar_width));

	/*
	GRASP.log("VerticalSplit("+(int)left+", "+(int)top+", "
		  +(int)width+", "+(int)height+")@"
		  +(int)center
		  +"; fph="+(int)firstPanel.height);*/

    }

    @Override
    public Panel copy() {
	return new VerticalSplit(left(), top(),
				 width(), height(),
				 firstPanel, secondPanel);
    }

    @Override
    public void render(Canvas canvas) {

	canvas.save();

	canvas.drawRect(0, firstPanel.height(),
			firstPanel.width(),
			firstPanel.height() + bar_width,
			GRASP.paint);

	
	canvas.clipRect(0, 0,
			firstPanel.width(),
			firstPanel.height());
	firstPanel.render(canvas);
	canvas.restore();

	canvas.save();
	canvas.translate(0, (firstPanel.height() + bar_width));
	canvas.clipRect(0, 0,
			secondPanel.width(),
			secondPanel.height());
	secondPanel.render(canvas);
	canvas.restore();
    }

    @Override
    public String toString() {
	return "VS("+firstPanel.toString()
	    +", "+secondPanel.toString()+")";
    }

    @Override
    public Drag onPress(Screen screen,
			int finger,
			float x, float y) {
	
	if (firstPanel.height() < y
	    && y < firstPanel.height() + bar_width) {
	    return translate(super
			     .onPress(screen, finger,
				      x, y-firstPanel.height()),
			     0, firstPanel.height());
	}
	if (y <= firstPanel.height()) {
	    return firstPanel.onPress(screen, finger, x, y);
	}
	if (y >= firstPanel.height() + bar_width) {
	    return translate(secondPanel
			     .onPress(screen, finger,
				      x,
				      y-firstPanel.height()-bar_width
				      ),
			     0, firstPanel.height()+bar_width);
	}
	assert(false);
	return null;
    }

    @Override
    public void onClick(Screen screen,
			int finger,
			float x, float y) {
	if (firstPanel.height() < y
	    && y < firstPanel.height() + bar_width) {
	    super.onClick(screen, finger,
			  x, y-firstPanel.height());
	    return;
	}
	if (y <= firstPanel.height()) {
	    firstPanel.onClick(screen, finger, x, y);
	    return;
	}
	if (y >= firstPanel.height() + bar_width) {
	    secondPanel.onClick(screen, finger,
				x, y-firstPanel.height()-bar_width);
	    return;
	}
	assert(false);
    }


    @Override
    public Drag onSecondPress(Screen screen,
			      int finger,
			      float x, float y) {
	if (firstPanel.height() < y
	    && y < firstPanel.height() + bar_width) {
	    return translate(super
			     .onSecondPress(screen, finger, x,
					    y-firstPanel.height()),
			     0, firstPanel.height());
	}
	if (y <= firstPanel.height()) {
	    return firstPanel.onSecondPress(screen, finger, x, y);
	}
	if (y >= (firstPanel.height()+bar_width)) {
	    return translate(secondPanel
			     .onSecondPress(screen, finger,
					    x,
					    y-(firstPanel.height()
					       +bar_width)),
			     0, (firstPanel.height()+bar_width));
	}
	assert(false);
	return null;
    }

    @Override
    public void onDoubleClick(Screen screen,
			      int finger,
			      float x, float y) {
	if (firstPanel.height() < y
	    && y < (firstPanel.height()+bar_width)) {
	    super.onDoubleClick(screen, finger,
				x, y-firstPanel.height());
	    return;
	}
	if (y <= firstPanel.height()) {
	    firstPanel.onDoubleClick(screen, finger, x, y);
	    return;
	}
	if (y >= (firstPanel.height()+bar_width)) {
	    secondPanel.onDoubleClick(screen, finger,
				      x, y-(firstPanel.height()
					    +bar_width));
	    return;
	}
	assert(false);
    }

    @Override
    public Drag onHold(Screen screen,
		       int finger,
		       float x, float y) {
	if (firstPanel.height() < y
	    && y < (firstPanel.height()+bar_width)) {
	    return translate(super
			     .onHold(screen, finger, x,
				     y-firstPanel.height()),
			     0, firstPanel.height());
	}
	if (y <= firstPanel.height()) {
	    return firstPanel.onHold(screen, finger, x, y);
	}
	if (y >= (firstPanel.height()+bar_width)) {
	    return translate(secondPanel
			     .onHold(screen, finger,
				     x, y-(firstPanel.height()
					   +bar_width)),
			     0, (firstPanel.height()+bar_width));
	}
	assert(false);
	return null;
    }

    @Override
    public Panel
	finishResizing(Split s, float vx, float vy) {
	if (s == this) {
	    if (vy > closing_threshold
		|| secondPanel.height() <= bar_width) {
		firstPanel.setHeight(height());
		firstPanel.setTop(top());
		return firstPanel;
	    }
	
	    if (vy < -closing_threshold
		|| firstPanel.height() <= bar_width) {
		secondPanel.setHeight(height());
		secondPanel.setTop(top());
		return secondPanel;
	    }
	}

	assert(firstPanel.bottom() < secondPanel.top());
	
	if (s.bottom() <= firstPanel.bottom()) {
	    firstPanel = firstPanel.finishResizing(s, vx, vy);
	}
	else if (s.top() >= secondPanel.top()) {
	    secondPanel =
		secondPanel.finishResizing(s, vx, vy);
	}
	return this;
    }

    @Override
    public void resizeBy(float dx, float dy) {
	firstPanel.setHeight(firstPanel.height() + dy);
	secondPanel.setTop(secondPanel.top() + dy);
	secondPanel.setHeight(secondPanel.height() - dy);
    }

    @Override
    public void setLeft(float v) {
	super.setLeft(v);
	firstPanel.setLeft(v);
	secondPanel.setLeft(v);
    }

    @Override
    public void setTop(float v) {
	super.setTop(v);
	firstPanel.setTop(v);
	secondPanel.setTop(v + firstPanel.height()
			       + bar_width);
    }

    @Override
    public void setWidth(float v) {
	super.setWidth(v);
	firstPanel.setWidth(v);
	secondPanel.setWidth(v);
    }

    @Override
    public void setHeight(float h0_) {
	float h0 = height();
	float h1 = firstPanel.height();
	float h2 = secondPanel.height();
	assert(h0 == h1 + h2 + bar_width);
	float b2 = bar_width/2.0f;

	float h1_ = h0_*(h1+b2)/h0 - b2;
	float h2_ = h0_ - h1_ - bar_width;
	
	super.setHeight(h0_);
	firstPanel.setHeight(h1_);
	secondPanel.setTop(top() +firstPanel.height()+bar_width);
	secondPanel.setHeight(h2_);
    }

    @Override
    public Panel at(float x, float y) {
	if (y <= firstPanel.height()) {
	    return firstPanel.at(x, y);
	}
	else if (y >= firstPanel.height() + bar_width) {
	    return secondPanel.at(x, y - (firstPanel.height()
				       + bar_width));
	}
	return super.at(x, y - firstPanel.height());
    }

    @Override
    public boolean insertAt(float x, float y, Bit bit) {
	if (y <= firstPanel.height()) {
	    return firstPanel.insertAt(x, y, bit);
	}
	else if (y >= firstPanel.height() + bar_width) {
	    return secondPanel.insertAt(x,
					y - (firstPanel.height()
					     + bar_width),
					bit);
	}
	return false;
    }

    
}
