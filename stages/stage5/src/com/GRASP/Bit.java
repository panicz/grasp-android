package com.GRASP;

import android.graphics.Canvas;

/*


Bit = Atom :following_space (Space|null) 
           :text String
    | Box :following_space (Space|null) 
          :first_interline Interline;

Space = Space :width float 
              :following_bit (Bit|null);

Interline = Interline :height float 
                      :following_line (Line|null);

Line = Line :first_space (Space|null) 
            :next_interline (Interline|null);

*/
interface Bit extends Tile {
    /* following_space should be a field, but Java interfaces
       won't allow it, and we cannot make it an abstract class
       for other reasons */
    Space following_space();
    void set_following_space(Space s);

    void render(Canvas canvas);

    float width();
    float height();

    float min_width();
    float min_height();

    float overwidth();
    float overheight();
    
    int buildString(StringBuilder sb, int indent);
    int buildString(StringBuilder sb);

    Space insertAt(float x, float y,
		   DragAround item,
		   Ref<Line> ln);

    Bit itemAt(float x, float y);
    //  Bit takeFrom(float x, float y);

    Drag dragAround(float x, float y, TakeBit take);
    
    Bit shallow_copy();
    Bit deep_copy();
    
}




