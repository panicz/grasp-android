package com.GRASP;

import android.view.KeyEvent;

class AtomEditor extends TextInput {
    Atom target;
    public AtomEditor(Atom atom) {
	super(300, atom.text, Atom.text_size,
	      GRASP.symbols_font);
	target = atom;
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
	   target.text = getText();
	   return true;
       }
       return result;
    }
}
