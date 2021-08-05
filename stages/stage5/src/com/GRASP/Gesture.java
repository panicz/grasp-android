package com.GRASP;

abstract class Gesture {
    public String name;
    public abstract boolean recognizes(Shape shape, Screen screen);
    public abstract void perform(Shape shape, Screen screen);
}
