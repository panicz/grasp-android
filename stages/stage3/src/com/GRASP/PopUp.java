package com.GRASP;

import android.graphics.Canvas;
import android.graphics.Color;
import java.util.List;

abstract class PopUp {

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

    protected PopUp(float x, float y, float w, float h) {
	left = x;
	top = y;
	width = w;
	height = h;
    }

    abstract void drawContents(Canvas canvas);

    public void draw(Canvas canvas) {
	GRASP.paint.setColor(Color.BLACK);
	GRASP.paint.setAlpha(64);

	canvas.drawRoundRect(left, top,
			     left+width, top+height,
			     radius, radius,
			     GRASP.paint);
	drawContents(canvas);
    }

    abstract public PopUp onClick(float x, float y);

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
	public PopUp to(Screen screen, float x, float y,
		       float vx, float vy){
	    return target;
	}

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
	public PopUp to(Screen screen, float x, float y,
		       float vx, float vy) {
	    return null;
	}
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
	public PopUp to(Screen screen, float x, float y,
		       float vx, float vy) {
	    return null;
	}
    };

    
    abstract public Skim skim(float x, float y,
			      float w, float h);
    
}
