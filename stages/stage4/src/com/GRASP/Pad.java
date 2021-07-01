package com.GRASP;


interface Pad extends Tile {

    Drag onPress(Screen screen, byte finger, float x, float y);
    void onClick(Screen screen, byte finger, float x, float y);
    Drag onSecondPress(Screen screen, byte finger, float x, float y);
    void onDoubleClick(Screen screen, byte finger, float x, float y);
    Drag onHold(Screen screen, byte finger, float x, float y);

}
