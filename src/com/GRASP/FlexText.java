package com.GRASP;

import android.graphics.Canvas;
import android.view.KeyEvent;

import android.graphics.Paint;
//import android.graphics.TextPaint;
import android.graphics.Color;
import android.graphics.Typeface;


class FlexText extends IdleBox {
    
    public static class TextData {
	public StringBuilder caption
	    = new StringBuilder();
	
	public int cursor = 0;
	public int selection = 0;
	public Paint paint;
	// assert 0 <= cursor < caption.length()
	// assert 0 <= cursor + selection
	//  < caption.length()
	public TextData(Typeface font,
			int fontsize,
			int color,
			int bgcolor) {
	    paint = new Paint();
	    paint.setTypeface(font);
	    paint.setTextSize(fontsize);
	    paint.setColor(color);
	    paint.setStrokeWidth(4);
	    //paint.bgcolor = bgcolor;
	}

	public TextData() {
	    this(GRASP.default_font, 36,
		 Color.BLACK, Color.WHITE);
	}

	public void invertColors() {
	    /*
	      int color = paint.getColor();
	      int bgcolor = paint.bgcolor;
	      paint.bgcolor = color;
	      paint.setColor(bgcolor);*/
	}

	public void draw(Canvas c) {

	    if (selection < 0) {
			
		String prefix = caption
		    .substring(0,
			       cursor
			       +selection);
		c.drawText(prefix,
			   0, paint.getTextSize(),
			   paint);
		//draw cursor
		invertColors();

		float prefix_width =
		    paint.measureText(prefix);
			
		String selected = caption
		    .substring(cursor
			       +selection,
			       cursor);

		c.drawText(selected,
			   prefix_width,
			   paint.getTextSize(),
			   paint);

		invertColors();
			
		float selection_width =
		    paint.measureText(selected);

		String suffix = caption
		    .substring(cursor);

		c.drawText(suffix,
			   prefix_width
			   +selection_width,
			   paint.getTextSize(),
			   paint);
	    }
	    else /*if (t.selection >= 0)*/ {
		String prefix = caption
		    .substring(0,
			       cursor);
		c.drawText(prefix,
			   0, paint.getTextSize(),
			   paint);
		invertColors();
			
		float prefix_width =
		    paint.measureText(prefix);
			
		String selected = caption
		    .substring(cursor,
			       cursor
			       +selection);

		c.drawText(selected,
			   prefix_width,
			   paint.getTextSize(),
			   paint);

		invertColors();
		//draw cursor
			
		float selection_width =
		    paint.measureText(selected);
			
		String suffix = caption
		    .substring(cursor
			       +selection);

		c.drawText(suffix,
			   prefix_width
			   +selection_width,
			   paint.getTextSize(),
			   paint);

	    }
	}	
    }

    public static class DrawText implements DrawingMethod {
	@Override
	public void draw(Box b, Canvas c) {
	    if (b instanceof Flex) {
		Flex f = (Flex) b;
		if (f.data instanceof TextData) {
		    TextData t = (TextData) f.data;
		    t.draw(c);
		}
	    }
	}
    };

    public static class EditText implements TypeHandler {
	TextData edit;
	
	@Override
	public ActionResult action(KeyEvent event) {
	    int code = event.getKeyCode();
	    
	    //GRASP.Log(KeyEvent.keyCodeToString(code));

	    if (event.isPrintingKey()) {
		int u = event.getUnicodeChar();
		String s = Character.toString((char) u);
		if (edit.selection < 0) {
		    edit.caption
			.replace(edit.selection
				 +edit.cursor,
				 edit.cursor,
				 s);
		}
		else {
		    edit.caption
			.replace(edit.cursor,
				 edit.cursor
				 +edit.selection,
				 s);
		}
		edit.selection = 0;
		edit.cursor++;

	    }
	    else {
		switch (code) {
		case KeyEvent.KEYCODE_DPAD_LEFT:
		    if (event.isShiftPressed()) {
			if (--edit.selection
			    < -edit.cursor) {
			    edit.selection =
				-edit.cursor;
			}
		    }
		    else {
			if (--edit.cursor < 0) {
			    edit.cursor = 0;
			}
			edit.selection = 0;
		    }
		    break;
		case KeyEvent.KEYCODE_DPAD_RIGHT:
		    if (event.isShiftPressed()) {
			if (++edit.selection
			    + edit.cursor
			    > edit.caption.length()) {
			    edit.selection =
				edit.caption.length()
				- edit.cursor;
			}
		    }
		    else {
			if (++edit.cursor >
			    edit.caption.length()) {
			    edit.cursor =
				edit.caption.length();
			}
		    }
		    break;

		case KeyEvent.KEYCODE_DEL: //backspace
		    if (edit.selection < 0) {
			edit.caption
			    .replace(edit.selection
				     +edit.cursor,
				     edit.cursor,
				     "");
			edit.selection = 0;
		    }
		    else if (edit.selection > 0) {
			edit.caption
			    .replace(edit.cursor,
				     edit.cursor
				     +edit.selection,
				     "");
			edit.selection = 0;
		    }
		    else if (edit.cursor > 0) {
			edit.caption
			    .replace(edit.cursor-1,
				     edit.cursor,
				     "");
			--edit.cursor;
		    }
		    break;
		    
		case KeyEvent.KEYCODE_FORWARD_DEL: //del
		    if (edit.selection < 0) {
			edit.caption
			    .replace(edit.selection
				     +edit.cursor,
				     edit.cursor,
				     "");
			edit.selection = 0;
		    }
		    else if (edit.selection > 0) {
			edit.caption
			    .replace(edit.cursor,
				     edit.cursor
				     +edit.selection,
				     "");
			edit.selection = 0;
		    }
		    else if (edit.caption.length()
			     >= edit.cursor) {
			edit.caption
			    .replace(edit.cursor,
				     edit.cursor+1,
				     "");
		    }
		    break;
		}
	    }
	    return ActionProcess;
	}

	public EditText(TextData e) {
	    edit = e;
	}
    }

    public static DrawingMethod drawText = new DrawText();

    public static class FlexToText implements TouchHandler {
	public Flex target;
	//public ListBox source;
	
	@Override
	public ActionResult action(float x, float y) {
	    // trzeba odpowiednio ustawic wartosci
	    // callbackow targetu
	    
	    target.reaction.onSingleTap =
		showKeyboard;
	    
	    target.data = new TextData();
	    
	    target.onTypeKey =
		new EditText((TextData) target.data);

	    target.visualization = drawText;
	    
	    return Box.ActionProcess;
	}

	public FlexToText(Flex parent){//,ListBox remove){
	    target = parent;
	    // source = remove;
	}
    }

};
