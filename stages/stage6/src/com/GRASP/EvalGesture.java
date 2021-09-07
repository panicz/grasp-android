package com.GRASP;


class EvalGesture extends Gesture {

    float epsilon;
    public EvalGesture(float eps) {
	name = "eval";
	epsilon = eps;

    }

    Point p = new Point();

    Editor editor;
    DragAround target;
    
    @Override
    public boolean recognizes(Shape shape,
			      Screen screen) {

	if (shape.strokes.size() != 1) {
	    return false;
	}
	Stroke simplified = shape.strokes.get(0)
	    .simplify(epsilon);
	
	if (simplified.points.size() != 3) {
	    return false;
	}

	Point p0 = simplified.points.get(0).point;

	Panel panel = screen.panel.at(p0);

	if(!(panel instanceof Panel)) {
	    GRASP.log(panel.toString());
	    return false;
	}

	Point p1 = simplified.points.get(1).point;
	
	if (screen.panel.at(p1) != panel) {
	    return false;
	}
	Point p2 = simplified.points.get(2).point;

	if (screen.panel.at(p2) != panel) {
	    return false;
	}

	if (!(p0.x < p1.x
	      && p1.y < p0.y
	      && p1.y < p2.y)) {
	    return false;
	}

	p.set(p1);

	editor = (Editor) panel;
	
	p.x -= editor.left();
	p.y -= editor.top();
	p.untransform(editor.transform);

	target = editor.document.topLevelItemAt(p.x, p.y);

	if (target == null) {
	    editor = null;
	    return false;
	}
	
	return true;
    }

    @Override
    public void perform(Shape shape, Screen screen) {
	GRASP.log(target.target.toString());
	Lura.eval(target.target, editor);
    }    

    
}
