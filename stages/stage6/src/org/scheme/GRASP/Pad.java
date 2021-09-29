package org.scheme.GRASP;


interface Pad extends Tile {

    Drag onPress(Screen screen, byte finger,
		 float x, float y);
    void onClick(Screen screen, byte finger,
		 float x, float y);
    Drag onSecondPress(Screen screen, byte finger,
		       float x, float y);
    void onDoubleClick(Screen screen, byte finger,
		       float x, float y);
    Drag onHold(Screen screen, byte finger,
		float x, float y);

    boolean onKeyUp(Screen screen, int keycode,
		    char unicode, int meta);

    boolean onKeyDown(Screen screen, int keycode,
		      char unicode, int meta);

    // these are only triggered when the parent decides so
    // (currently only triggered from Popup and Below,
    // and handled by Below and Button)
    void onDragOver(Screen screen, byte finger,
		    float x, float y);
    void onDragOut(Screen screen, byte finger);
    void onRelease(Screen screen, byte finger,
		   float x, float y);

    void onRemove(Screen screen);
}
