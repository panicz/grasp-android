package com.GRASP;

import android.graphics.Canvas;
import android.graphics.Color;
import java.util.List;

class Choices extends PopUp {
    public Button [] buttons = new Button[0];

    public int highlighted = -1;

    public final int text_size = 48;
    
    public Choices(Button... buttons) {
	super(0, 0, 300, Button.height * buttons.length
	      + 2*(radius+margin));
	this.buttons = buttons;

	GRASP.paint.setTextSize(text_size);
	for (int i = 0; i < buttons.length; ++i) {
	    float w = 4*margin 
		+ GRASP.paint.measureText(buttons[i].label);
	    if (w > width) {
		width = w;
	    }
	}
    }
    
    public Choices(List<Button> buttons) {
	this(buttons.toArray(new Button[buttons.size()]));
    }

    @Override
    public void drawContents(Canvas canvas) {
	GRASP.paint.setColor(Color.WHITE);
	GRASP.paint.setAlpha(200);
	GRASP.paint.setTextSize(text_size);

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

    @Override
    public PopUp onClick(float x, float y) {
	switch(area(x, y)) {		
	case BottomLeft:		
	case BottomRight:
	case Top:
	    return this;

	case Inside:
	    return buttons[button_index(y)]
		.action.perform();
	    
	case Outside:
	default:
	    return null;
	}
    }

    public int button_index(float y) {
	return (int)
	    ((y - top - radius - margin)/Button.height);
    }

    class Highlight implements Skim {
	Choices target;

	public Highlight(Choices target) {
	    this.target = target;
	}

	@Override
	public void through(float x, float y,
			    float dx, float dy) {
	    if (target.area(x, y) != PopUp.Area.Inside) {
		target.highlighted = -1;
	    }
	    else {
		target.highlighted =
		    target.button_index(y);
	    }
	}

	@Override
	public PopUp to(Screen screen, float x, float y,
			float vx, float vy) {

	    int highlighted = target.button_index(y);
	    if(highlighted != target.highlighted) {
		target.highlighted = highlighted;
	    }
	    
	    if (0 <= highlighted
		&& highlighted < target.buttons.length) {
		return target.buttons[highlighted]
		    .action.perform();
	    }
	    return target;
	}
    };

    @Override
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
