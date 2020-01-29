package com.slayer;

import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import android.graphics.Canvas;
import android.graphics.Paint;

class Logger {
    Deque<String> lines =
	new LinkedList<String>();
    int max_size;
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
    public Logger(int size) {
	max_size = size;
    }
    public void draw(Canvas canvas,
		     float x, float y,
		     Paint paint) {
	Iterator<String> it =
	    lines.iterator();
	int i = 0;
	float h = paint.getTextSize()+1;
	while (it.hasNext()) {
	    canvas.drawText(it.next(),
			    x,
			    y + h*i++,
			    paint);
	}
    }
}
