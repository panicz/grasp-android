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

	secondPanel.setTop(center + bar_width/2.0f);
	secondPanel.setHeight(height() - firstPanel.height()
			      - bar_width);

	secondPanel.scrollBy(0, firstPanel.height()
			     + bar_width);

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
	firstPanel.render(canvas);
	canvas.drawRect(firstPanel.left(),
			firstPanel.bottom(),
			firstPanel.right(),
			secondPanel.top(),
			GRASP.paint);
	secondPanel.render(canvas); 
    }

    @Override
    public String toString() {
	return "VS("+firstPanel.toString()
	    +", "+secondPanel.toString()+")";
    }

    @Override
    public Split splitUnder(float x, float y) {
	
	if (firstPanel.bottom() < y && y < secondPanel.top()) {
	    return this;
	}
	if (y <= firstPanel.bottom()) {
	    return firstPanel.splitUnder(x, y);
	}
	if (y >= secondPanel.top()) {
	    return secondPanel.splitUnder(x, y);
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
	secondPanel.setTop(top() + firstPanel.height()
			   + bar_width);
	secondPanel.setHeight(h2_);
    }    
}
