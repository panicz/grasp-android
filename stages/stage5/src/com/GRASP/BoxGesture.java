package com.GRASP;


class BoxGesture extends Gesture {

    float epsilon;
    public BoxGesture(float eps) {
	name = "box";
	epsilon = eps;

    }

    Point a = new Point();
    Point b = new Point();
    Point c = new Point();
    Point d = new Point();

    float top, left, bottom, right;
    Editor editor;
    
    @Override
    public boolean recognizes(Shape shape,
			      Screen screen) {

	if (shape.strokes.size() != 1) {
	    return false;
	}
	Stroke simplified = shape.strokes.get(0)
	    .simplify(epsilon);
	
	if (simplified.points.size() != 5) {
	    GRASP.log("simplified.points.size() = "
		      +simplified.points.size());
	    return false;
	}
	Point p1 = simplified.points.get(0).point;

	Panel target = screen.panel.at(p1);

	if(!(target instanceof Editor)) {
	    GRASP.log(target.toString());
	    return false;
	}
	
	Point p5 = simplified.points.get(4).point;

	if (screen.panel.at(p5) != target) {
	    return false;
	}
	
	float d0 = p5.distanceTo(p1);

	if (d0 > 100) {
	    GRASP.log("d0 = "+d0);
	    return false;
	}

	    
	Point p2 = simplified.points.get(1).point;

	if (screen.panel.at(p2) != target) {
	    return false;
	}
	
	Point p3 = simplified.points.get(2).point;	

	if (screen.panel.at(p3) != target) {
	    return false;
	}


	Point p4 = simplified.points.get(3).point;
	
	if (screen.panel.at(p4) != target) {
	    return false;
	}
		
	editor = (Editor) target;
	
	float l1 = slope(p1, p2);
	float l2 = slope(p2, p3);
	float l3 = slope(p3, p4);
	float l4 = slope(p4, p5);

	float margin = 15.0f;

	float rotation = editor.transform.getAngle();
	
	if (!(near(l1, rotation, margin)
	      || near(l2, rotation, margin)
	      || near(l3, rotation, margin)
	      || near(l4, rotation, margin))) {
	    //*
	    GRASP.log("l1 = "+l1+", "
		      +"l2 = "+l2+", "
		      +"l3 = "+l3+", "
		      +"l4 = "+l4);//*/
	    return false;
	}
	
	if(!((near(angle(l2 - l1), 90.0f, margin)
	      && near(angle(l3 - l2), 90.0f, margin)
	      && near(angle(l4 - l3), 90.0f, margin))
	     || (near(angle(l2 - l1), -90.0f, margin)
		 && near(angle(l3 - l2), -90.0f, margin)
		 && near(angle(l4-l3),-90.0f,margin)))) {
	    //*
	    GRASP.log("l2-l1 = "+angle(l2-l1)+", "
		      +"l3-l2 = "+angle(l3-l2)+", "
		      +"l4-l3 = "+angle(l4-l3));//*/
	    return false;
	}

	a.set(p1);
	a.untransform(editor.transform);
	b.set(p2);
	b.untransform(editor.transform);
	c.set(p3);
	c.untransform(editor.transform);
	d.set(p4);
	d.untransform(editor.transform);
	
	top = min(a.y, b.y, c.y, d.y);
	left = min(a.x, b.x, c.x, d.x);

	bottom = max(a.y, b.y, c.y, d.y);
	right = max(a.x, b.x, c.x, d.x);
	
	return true;
    }

    @Override
    public void perform(Shape shape, Screen screen) {
	// musimy:
	// znalec element w editor.document zawierajacy 
	// wszystkie punkty
	editor.document.createBox(left, top,
				  right, bottom);
	
    }
}
