package com.GRASP;

interface Drag {
    void move(Layers layers, float x, float y,
	      float dx, float dy);
    void drop(Layers layers, float x, float y,
	      float vx, float vy);
};
