package com.slayer;

import android.graphics.Canvas;
import android.graphics.Paint;

import java.io.Serializable;

class Box implements Asset {

    Point tl;
    Point br;
    Point tr;
    Point bl;

    
    public Box(Shape s) {
	tl = new Point(s.left, s.top);
	br = new Point(s.right, s.bottom);
	tr = new Point(s.right, s.top);
	bl = new Point(s.left, s.bottom);

    }

    public void draw(Canvas canvas,
		     Paint paint,
		     Transform t) {
	Point ttl = t.p(tl);
	Point tbr = t.p(br);
	Point ttr = t.p(tr);
	Point tbl = t.p(bl);

	canvas.drawLine(ttl.x, ttl.y,
			tbl.x, tbl.y,
			paint);
	canvas.drawLine(tbl.x, tbl.y,
			tbr.x, tbr.y,
			paint);
	canvas.drawLine(
			tbr.x, tbr.y,
			ttr.x, ttr.y,
			paint);
	canvas.drawLine(
			ttr.x, ttr.y,
			ttl.x, ttl.y,
			paint);

    }
    
}
