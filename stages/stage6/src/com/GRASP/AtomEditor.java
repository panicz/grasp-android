package com.GRASP;

import android.view.KeyEvent;

class AtomEditor extends TextInput {
    Space ps;
    Line line;
    public AtomEditor(Space ps, Line ln) {
	super(300,
	      ((Atom) ps
	       .following_bit).text,
	      Atom.text_size,
	      GRASP.symbols_font);
	this.ps = ps;
	line = ln;
    }

    @Override
    public boolean onKeyDown(Screen screen, int keycode,
			     char unicode, int meta) {
	
       if (keycode == KeyEvent.KEYCODE_ENTER
	   || keycode == KeyEvent.KEYCODE_SPACE) {
	   return true;
       }
       boolean result =
	   super.onKeyDown(screen, keycode,
			   unicode, meta);
       if (unicode != 0
	   || keycode == KeyEvent.KEYCODE_DEL
	   || keycode == KeyEvent.KEYCODE_FORWARD_DEL) {
	   ((Atom)ps.following_bit).text = getText();
	   return true;
       }
       return result;
    }

    @Override
    public void onRemove(Screen screen) {
	/// usuwamy puste atomy
	if (getText().length() == 0) {
	    ps.remove_following_bit(line);
	}
    }

}
