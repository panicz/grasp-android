package com.GRASP;

class UnderscoreGesture extends Gesture {

    float epsilon;
    public UnderscoreGesture(float eps) {
	name = "underscore";
	epsilon = eps;

    }

    float top, left;
    Editor editor;
    
    @Override
    public boolean recognizes(Shape shape,
			      Screen screen) {
	if (shape.strokes.size() != 1) {
	    return false;
	}
	Stroke simplified = shape.strokes.get(0)
	    .simplify(epsilon);
	
	if (simplified.points.size() != 4) {
	    /*GRASP.log("simplified.points.size() = "
	      +simplified.points.size());*/
	    return false;
	}

	Point p1 = simplified.points.get(0).point;
	left = p1.x;
	top = p1.y;

	Panel target = screen.panel.at(p1);

	if(!(target instanceof Editor)) {
	    GRASP.log(target.toString());
	    return false;
	}
	if (!(target instanceof Editor)) {
	    return false;
	}
	
	Point p2 = simplified.points.get(1).point;

	if(screen.panel.at(p2) != target) {
	    return false;
	}
	    
	Point p3 = simplified.points.get(2).point;

	if(screen.panel.at(p3) != target) {
	    return false;
	}

	Point p4 = simplified.points.get(3).point;

	if(screen.panel.at(p4) != target) {
	    return false;
	}

	
	if (!(p2.y > top
	      && p3.x > left
	      && p3.x > p2.x
	      && p4.y < p3.y
	      && p4.x > left
	      && p4.x > p2.x)) {
	    return false;
	}
	editor = (Editor) target;
	return true;
    }

    @Override
    public void perform(Shape shape, Screen screen) {
	// add a new empty atom
    }

    
}
