package com.slayer;

import android.graphics.Canvas;
import android.graphics.Paint;
import java.io.Serializable;

interface Asset extends Serializable {
    public void draw(Canvas canvas,
		     Paint paint,
		     Transform transform);

    
}
