package com.GRASP;

import android.graphics.Canvas;
import android.graphics.Path;


class Box extends Bit {
    /*@NonNull*/ public Interline first_interline = null;

    final static float parenWidth = 20;
    final static float parenBar = 20;

    Path _leftParen = null;
    float _leftParenHeight;
    
    Path _rightParen = null;
    float _rightParenHeight;
    
    Path leftParen(float h) {

	if(h != _leftParenHeight
	   || _leftParen == null) {
	    Path path = new Path();

	    path.moveTo(20, 0);
	    path.quadTo(5, 0, 0, 50);
	    path.lineTo(10, 50);
	    path.quadTo(10, 30, 20, 30);
	    //path.lineTo(120, 100);
	    path.close();

	    path.addRect(0, 50, 10, h+50, Path.Direction.CCW);
	    path.moveTo(20, h+100);
	    path.quadTo(5, h+100, 0, h+50);
	    path.lineTo(10, h+50);
	    path.quadTo(10, h+70, 20, h+70);
	    path.close();

	    _leftParen = path;
	    _leftParenHeight = h;
	}
	return _leftParen;
    }

    Path rightParen(float h) {
	if(h != _rightParenHeight
	   || _rightParen == null) {

	    Path path = new Path();

	    //path.moveTo(20, 0);
	    path.quadTo(15, 0, 20, 50);
	    path.lineTo(10, 50);
	    path.quadTo(10, 30, 0, 30);
	    //path.lineTo(120, 100);
	    path.close();

	    path.addRect(20, 50, 10, h+50, Path.Direction.CW);
	    path.moveTo(0, h+100);
	    path.quadTo(15, h+100, 20, h+50);
	    path.lineTo(10, h+50);
	    path.quadTo(10, h+70, 0, h+70);
	    path.close();

	    _rightParen = path;
	    _rightParenHeight = h;
	
	}
	return _rightParen;
    }
    
    @Override
    public void render(Canvas canvas) {

	float accumulated_height = 0;

	Interline interline;

	float maximum_width = 0;

	int previous_color = GRASP.paint.getColor();
	GRASP.paint.setColor(previous_color + 0x111111);
	
	for (interline = first_interline;
	     interline != null
		 && interline.following_line != null;
	     interline = interline.following_line.next_interline) {
	    Line line = interline.following_line;

	    float line_height = line.height();
	    
	    accumulated_height += interline.height;

	    float accumulated_width = parenWidth;
	    float maximum_height = 0;

	    
	    for (Space preceding_space = line.first_space;
		 preceding_space != null
		     && preceding_space.following_bit != null;
		 preceding_space =
		     preceding_space.following_bit.following_space) {

		Bit bit = preceding_space.following_bit;
	       
		accumulated_width += preceding_space.width;

		float w = bit.width();
		float h = bit.height();

		assert(h <= line_height);
		
		float dh = (line_height - h);

		if (h > maximum_height)  {
		    maximum_height = h;
		}
		
		canvas.save();
		canvas.translate(accumulated_width,
				 accumulated_height);
		canvas.clipRect(0,0,w,h+60);
		bit.render(canvas);
		canvas.restore();
		
		accumulated_width += w;
	    }
	    if (accumulated_width > maximum_width) {
		maximum_width = accumulated_width;
	    }
	    
	    accumulated_height += maximum_height;
	}
	
	canvas.drawPath(leftParen(accumulated_height - 60),
			GRASP.paint);
	canvas.save();
	canvas.translate(maximum_width, 0);
	canvas.drawPath(rightParen(accumulated_height - 60),
			GRASP.paint);
	canvas.restore();
	GRASP.paint.setColor(previous_color);
    }

    
    @Override public float width() {
	return 2*parenWidth+first_interline.maximum_width();
    }
    
    @Override public float height() {
	return first_interline.onward_height();
    }
}
