package com.GRASP;

import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import android.graphics.Canvas;

class Logger {
    Deque<String> lines =
	new LinkedList<String>();
    int max_size;

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

    public void update(String msg) {
	if (lines.size() > 0) {
	    lines.removeFirst();
	}	    
	log(msg);
    }
    
    public Logger(int size) {
	max_size = size;
    }

    public void clear() {
	lines.clear();
    }
    
    public void draw(Canvas canvas,
		     float x, float y) {
	Iterator<String> it =
	    lines.iterator();
	int i = 1;
	float h = GRASP.paint.getTextSize()+1;
	while (it.hasNext()) {
	    canvas.drawText(it.next(),
			    x,
			    y + h*i++,
			    GRASP.paint);
	}
    }
}
