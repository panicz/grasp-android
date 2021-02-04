package com.GRASP;

import android.graphics.Canvas;
import android.graphics.Color;


class PopUp {

    public enum Area {
	Outside,
	Top,
	BottomLeft,
	BottomRight,
	Inside
    };
    
    public static final float radius = 25.0f;
    public static final float margin = 5.0f;
    
    public float left, top, width, height;
    public Button [] buttons = new Button[0];

    public int highlighted = -1;
    
    public PopUp(float x, float y, float w, float h) {
	left = x;
	top = y;
	width = w;
	height = h;
    }

    public PopUp(Button... buttons) {
	this.buttons = buttons;
	left = 0;
	top = 0;
	width = 300;
	height = Button.height * buttons.length
	    + 2*(radius+margin);
    }    
    
    public void draw(Canvas canvas) {
	GRASP.paint.setAlpha(64);

	canvas.drawRoundRect(left, top,
			     left+width, top+height,
			     radius, radius,
			     GRASP.paint);
	GRASP.paint.setColor(Color.WHITE);
	GRASP.paint.setAlpha(200);


	/*
	canvas.drawRect(left+margin, top+radius+margin,
			left+width-margin,
			top+height-radius-margin,
			GRASP.paint);

	GRASP.paint.setColor(Color.BLACK);
	GRASP.paint.setTextSize(18);
	canvas.drawText("("+(int)left+", "+(int)top+", "
			+(int)width+", "+(int)height+")",
			left+8,
			top + height/2.0f - 18,
			GRASP.paint);
	*/
	GRASP.paint.setTextSize(48);

	for (int i = 0; i < buttons.length; ++i) {
	    GRASP.paint.setColor(i == highlighted
				 ? Color.BLACK
				 : Color.WHITE);
	    GRASP.paint.setAlpha(200);


	    canvas.drawRect(left+margin,
			    top+radius+margin
			    +i*Button.height+2,
			    left+width-margin,
			    top+radius+margin
			    +(i+1)*Button.height-2,
			    GRASP.paint);
	    GRASP.paint.setColor(i == highlighted
				 ? Color.WHITE
				 : Color.BLACK);
	    GRASP.paint.setAlpha(200);

	    canvas.drawText(buttons[i].label,
			    left+margin+6,
			    top+radius-16
			    +(i+1)*Button.height,
			    GRASP.paint);

	}
	GRASP.paint.setColor(Color.BLACK);
	GRASP.paint.setAlpha(255);

    }

    public PopUp onClick(float x, float y) {
	if (left <= x && x <= left+width
	    && top <= y && y <= top+height) {
	    return this;
	}
	else {
	    return null;
	}
    }

    public Area area(float x, float y) {
	if (x < left || left + width < x
	    || y < top || top+height < y) {
	    return Area.Outside;
	}
	
	if (y < top + radius + margin) {
	    return Area.Top;
	}
	else if (y > top + height - radius - margin) {
	    if (x - left < width/2.0f) {
		return Area.BottomLeft;
	    }
	    else {
		return Area.BottomRight;
	    }
	}
	else {
	    return Area.Inside;
	}
    }

    class MoveAround implements Skim {
	float screen_width;
	float screen_height;
	PopUp target;
	public MoveAround(PopUp target,
			  float w, float h) {
	    this.target = target;
	    screen_width = w;
	    screen_height = h;
	}
	
	@Override
	public void through(float x, float y,
			    float dx, float dy){
	    target.left += dx;
	    target.top += dy;
	    if (target.left + target.width > screen_width) {
		target.left -= target.left + target.width
		    - screen_width;
	    }
	    if (target.top + target.height > screen_height) {
		target.top -= target.top + target.height
		    - screen_height;
	    }

	}

	@Override
	public void to(Screen screen, float x, float y,
		       float vx, float vy){}

    };

    class ResizeBottomLeft implements Skim {
	PopUp target;

	public ResizeBottomLeft(PopUp target) {
	    this.target = target;	    
	}

	@Override
	public void through(float x, float y,
			    float dx, float dy){
	    target.width -= dx;
	    target.height += dy;
	    target.left += dx;
	}

	@Override
	public void to(Screen screen, float x, float y,
		       float vx, float vy){}
    };

    class ResizeBottomRight implements Skim {
	PopUp target;

	public ResizeBottomRight(PopUp target) {
	    this.target = target;
	}

	@Override
	public void through(float x, float y,
			    float dx, float dy){
	    target.width += dx;
	    target.height += dy;
	}

	@Override
	public void to(Screen screen, float x, float y,
		       float vx, float vy){}
    };

    class Highlight implements Skim {
	PopUp target;

	public Highlight(PopUp target) {
	    this.target = target;
	}

	@Override
	public void through(float x, float y,
			    float dx, float dy) {
	    if (target.area(x, y) != PopUp.Area.Inside) {
		target.highlighted = -1;
	    }
	    else {
		target.highlighted = (int)
		    ((y - target.top - PopUp.radius
		      - PopUp.margin)/Button.height);
	    }
	}

	@Override
	public void to(Screen screen, float x, float y,
		       float vx, float vy){}
    };

       
    
    public Skim skim(float x, float y,
		     float w, float h) {
	switch(area(x, y)) {
		
	case BottomLeft:
	    //return new ResizeBottomLeft(this);
		
	case BottomRight:
	    //return new ResizeBottomRight(this);

	case Top:
	    return new MoveAround(this, w, h);

	    
	case Inside:
	case Outside:
	default:
	    return new Highlight(this);
	}
    }
    
}
