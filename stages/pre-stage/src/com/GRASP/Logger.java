package com.GRASP;

import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Typeface;


class Logger {
    Deque<String> lines =
	new LinkedList<String>();
    int max_size;

    Paint paint;
    //Typeface font;
    //int font_size;
    public void log(Object... entries) {

	StringBuilder result =
	    new StringBuilder();
	for(int i = 0;
	    i < entries.length;
	    ++i) {
	    result.append(entries[i]);
	}
	lines.addFirst(result.toString());
	if (lines.size() > max_size) {
	    lines.removeLast();
	}
    }
    public Logger(int size, Typeface font, int fontsize,
		  int color) {
	max_size = size;
	paint = new Paint();
	//;font = f;
	paint.setTypeface(font);
	paint.setTextSize(fontsize);
	paint.setColor(color);

    }

    public void clear() {
	lines.clear();
    }
    
    public void draw(Canvas canvas,
		     float x, float y) {
	Iterator<String> it =
	    lines.iterator();
	int i = 1;

	float h = paint.getTextSize()+1;
	while (it.hasNext()) {
	    canvas.drawText(it.next(),
			    x,
			    y + h*i++,
			    paint);
	}
    }
}
