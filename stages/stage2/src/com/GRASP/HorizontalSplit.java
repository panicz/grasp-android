package com.GRASP;
import android.graphics.Canvas;
import android.graphics.RectF;

class HorizontalSplit extends Split {

    public HorizontalSplit(float x, float y, float w, float h,
			   Interactions left_panel,
			   Interactions right_panel) {
	super(x, y, w, h, left_panel, right_panel);
    }
    
    public HorizontalSplit(Interactions panel,
			   RectF rect) {
	super(panel.left(), panel.top(),
	      panel.width(), panel.height(),
	      panel, panel.copy());
	float center = (rect.left + rect.right)/2.0f;
	
	firstPanel.setWidth(center - left() - bar_width/2.0f);
	secondPanel.setLeft(center + bar_width/2.0f);
	secondPanel.setWidth(width() - firstPanel.width()
			     - bar_width);

	secondPanel.scrollBy(firstPanel.width()
			     + bar_width, 0);

	/*
	GRASP.log("HorizontalSplit("+(int)left+", "
		  +(int)top+", "
		  +(int)width+", "
		  +(int)height+")@"
		  +(int)center);*/
    }

    @Override
    public Interactions copy() {
	return new HorizontalSplit(left(), top(),
				   width(), height(),
				   firstPanel, secondPanel);
    }
    
    @Override
    public void render(Canvas canvas) {
	firstPanel.render(canvas);
	canvas.drawRect(firstPanel.right(),
			firstPanel.top(),
			secondPanel.left(),
			firstPanel.bottom(),
			GRASP.paint);
	secondPanel.render(canvas); 
    }

    @Override
    public String toString() {
	return "HS("+firstPanel.toString()
	    +", "+secondPanel.toString()+")";
    }

    @Override
    public Split splitUnder(float x, float y) {
	
	if (firstPanel.right() < x && x < secondPanel.left()) {
	    return this;
	}
	if (x <= firstPanel.right()) {
	    return firstPanel.splitUnder(x, y);
	}
	if (x >= secondPanel.left()) {
	    return secondPanel.splitUnder(x, y);
	}
	assert(false);
	return null;
    }

    
    @Override
    public Interactions
	finishResizing(Split s, float vx, float vy) {
	
	if (s == this) {
	    if (vx > closing_threshold
		|| secondPanel.width() <= bar_width) {
		firstPanel.setWidth(width());
		firstPanel.setLeft(left());
		return firstPanel;
	    }
	
	    if (vx < -closing_threshold
		|| firstPanel.width() <= bar_width) {
		secondPanel.setWidth(width());
		secondPanel.setLeft(left());
		return secondPanel;
	    }
	}

	assert(firstPanel.right() < secondPanel.left());
	
	if (s.right() <= firstPanel.right()) {
	    firstPanel = firstPanel.finishResizing(s, vx, vy);
	}
	else if (s.left() >= secondPanel.left()) {
	    secondPanel =
		secondPanel.finishResizing(s, vx, vy);
	}
	return this;
    }

    @Override
    public void resizeBy(float dx, float dy) {
	firstPanel.setWidth(firstPanel.width() + dx);
	secondPanel.setLeft(secondPanel.left() + dx);
	secondPanel.setWidth(secondPanel.width() - dx);
    }

    @Override
    public void setTop(float v) {
	super.setTop(v);
	firstPanel.setTop(v);
	secondPanel.setTop(v);
    }

    @Override
    public void setLeft(float v) {
	super.setLeft(v);
	firstPanel.setLeft(v);
	secondPanel.setLeft(v + firstPanel.width()
			   + bar_width);
    }

    @Override
    public void setHeight(float v) {
	super.setHeight(v);
	firstPanel.setHeight(v);
	secondPanel.setHeight(v);
    }

    @Override
    public void setWidth(float w0_) {
	float w0 = width();
	float w1 = firstPanel.width();
	float w2 = secondPanel.width();
	assert(w0 == w1 + w2 + bar_width);
	float b2 = bar_width/2.0f;

	float w1_ = w0_*(w1+b2)/w0 - b2;
	
	float w2_ = w0_ - w1_ - bar_width;
	
	super.setWidth(w0_);
	firstPanel.setWidth(w1_);
	secondPanel.setLeft(left() + firstPanel.width()
			   + bar_width);

	secondPanel.setWidth(w2_);
    }

}
