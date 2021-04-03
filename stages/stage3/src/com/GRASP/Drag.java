package com.GRASP;

interface Drag {
    void through(float x, float y, float dx, float dy);
    void to(Screen screen, float x, float y,
		    float vx, float vy);
};
