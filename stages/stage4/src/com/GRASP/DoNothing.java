package com.GRASP;

class DoNothing implements Procedure {
    public static Procedure instance = new DoNothing();

    @Override
    public void perform() {}
}
