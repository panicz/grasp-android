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
	super(panel.left, panel.top,
	      panel.width, panel.height,
	      panel, panel.copy());
	float center = (rect.left + rect.right)/2.0f;
	
	firstPanel.width = center - left - bar_width/2.0f;
	secondPanel.left = center + bar_width/2.0f;
	secondPanel.width = width - firstPanel.width
	    - bar_width;

	secondPanel.scrollBy(firstPanel.width + bar_width, 0);

	GRASP.log("HorizontalSplit("+(int)left+", "
		  +(int)top+", "
		  +(int)width+", "
		  +(int)height+")@"
		  +(int)center);
    }

    @Override
    public Interactions copy() {
	return new HorizontalSplit(left, top, width, height,
				   firstPanel, secondPanel);
    }
    
    @Override
    public void render(Canvas canvas) {
	firstPanel.render(canvas);
	canvas.drawRect(firstPanel.left + firstPanel.width,
			firstPanel.top,
			secondPanel.left,
			firstPanel.top + firstPanel.height,
			GRASP.paint);
	secondPanel.render(canvas); 
    }

    @Override
    public String toString() {
	return "HS("+firstPanel.toString()
	    +", "+secondPanel.toString()+")";
    }

}
