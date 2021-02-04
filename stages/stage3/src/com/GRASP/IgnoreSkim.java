package com.GRASP;

class IgnoreSkim implements Skim {
    public static IgnoreSkim instance = new IgnoreSkim();
    @Override
    public void through(float x, float y, float dx, float dy){}
    @Override
    public void to(Screen screen, float x, float y,
		   float vx, float vy){}
};
