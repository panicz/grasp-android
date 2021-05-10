package com.GRASP;

import android.graphics.Canvas;
import android.graphics.Path;
import android.graphics.Paint;

class Paren {

    public static class Left {
	static class TopCorner extends Path {
	    public TopCorner() {
		moveTo(20, 0);
		quadTo(5, 0, 0, 50);
		lineTo(10, 50);
		quadTo(10, 30, 20, 30);
		close();
	    }
	    public static final Path instance =
		new TopCorner();
	}

	static class BottomCorner extends Path {
	    public BottomCorner() {
		moveTo(20, 50);
		quadTo(5, 50, 0, 0);
		lineTo(10, 0);
		quadTo(10, 20, 20, 20);
		close();
	    }
	    public static final Path instance =
		new BottomCorner();
	}
	
	public static void render(Canvas canvas,
				  Paint paint,
				  float height) {
	    canvas.drawPath(TopCorner.instance, paint);
	    canvas.drawRect(0, 50, 10, height+50, paint);
	    canvas.save();
	    canvas.translate(0, height+50);
	    canvas.drawPath(BottomCorner.instance, paint);
	    canvas.restore();
	}
    }

    public static class Right {
	static class TopCorner extends Path {
	    public TopCorner() {
		quadTo(15, 0, 20, 50);
		lineTo(10, 50);
		quadTo(10, 30, 0, 30);
		close();
	    }
	    public static Path instance =
		new TopCorner();
	}

	static class BottomCorner extends Path {
	    public BottomCorner() {
		moveTo(0, 50);
		quadTo(15, 50, 20, 0);
		lineTo(10, 0);
		quadTo(10, 20, 0, 20);
		close();
	    }
	    public static Path instance =
		new BottomCorner();
	}

	public static void render(Canvas canvas,
				  Paint paint,
				  float height) {
	    canvas.drawPath(TopCorner.instance, paint);
	    canvas.drawRect(10, 50, 20, height+50, paint);
	    canvas.save();
	    canvas.translate(0, height+50);
	    canvas.drawPath(BottomCorner.instance, paint);
	    canvas.restore();
	}
	
    }
    
}
