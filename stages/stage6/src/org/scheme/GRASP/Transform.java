package org.scheme.GRASP;
import android.graphics.Canvas;

interface Transform {
    Transform canvas(Canvas target);
    float x(float ox, float oy);
    float y(float ox, float oy);

    Transform uncanvas(Canvas target);
    float unx(float mx, float my);
    float uny(float mx, float my);
    
    Transform anchor(float [] xs, float [] ys, byte n);
    Transform towards(float [] xs, float [] ys, byte n);

}
