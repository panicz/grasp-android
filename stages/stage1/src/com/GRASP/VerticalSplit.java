package com.GRASP;
import android.graphics.Canvas;
import android.graphics.RectF;

class VerticalSplit extends Split {

    public VerticalSplit(float x, float y, float w, float h,
			 Interactions top,
			 Interactions bottom) {
	super(x, y, w, h, top, bottom);
    }

    public VerticalSplit(Interactions panel,
			 RectF rect) {
	super(panel.left, panel.top,
	      panel.width, panel.height,
	      panel, panel.copy());
	float center = (rect.top + rect.bottom)/2.0f;
	firstPanel.height = center - top - bar_width/2.0f;

	secondPanel.top = center + bar_width/2.0f;
	secondPanel.height = height - firstPanel.height
	    - bar_width;

	secondPanel.scrollBy(0, firstPanel.height + bar_width);

	GRASP.log("VerticalSplit("+(int)left+", "+(int)top+", "
		  +(int)width+", "+(int)height+")@"
		  +(int)center
		  +"; fph="+(int)firstPanel.height);

    }

    @Override
    public Interactions copy() {
	return new VerticalSplit(left, top, width, height,
				 firstPanel, secondPanel);
    }

    
    @Override
    public void render(Canvas canvas) {
	firstPanel.render(canvas);
	canvas.drawRect(firstPanel.left,
			firstPanel.top + firstPanel.height,
			firstPanel.left + firstPanel.width,
			secondPanel.top,
			GRASP.paint);
	secondPanel.render(canvas); 
    }

    @Override
    public String toString() {
	return "VS("+firstPanel.toString()
	    +", "+secondPanel.toString()+")";
    }

}
