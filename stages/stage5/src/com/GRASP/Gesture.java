package com.GRASP;

abstract class Gesture {
    public String name;
    public abstract boolean recognize(Shape shape, Screen screen);
    public abstract boolean perform(Shape shape, Screen screen);
}
