package com.slayer;


import java.io.Serializable;


class Point implements Serializable {

    public float x;
    public float y;
    public Point(float left, float top) {
	x = left;
	y = top;
    }
}
