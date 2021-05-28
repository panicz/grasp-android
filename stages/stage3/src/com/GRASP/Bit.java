package com.GRASP;

import android.graphics.Canvas;

/*


Bit = Atom :rightward (Space|null) :inward String
    | Box :rightward (Space|null) :inward Interline;
Space = Space :width float :rightward (Bit|null);

Interline = Interline :height float :downward (Line|null);
Line = Line :rightward (Space|null) :downward (Interline|null);

SExp = SAtom :preceding_whitespace String :text String
    | SList :preceding_whitespace String
            :elements [SExp ...]
            :ultimate_whitespace String
    | SDotted :preceding_whitespace String
              :elements [SExp ...]
              :whitespace_before_dots String
              :tail SExp
              :ultimate_whitespace String;

bit : SExp -> Bit
bit (SAtom :preceding_whitespace ws :text s) = Atom :inward s
bit (SList :preceding_whitespace ws
           :elements xs
           :ultimate_whitespace us) =

ogolnie jest tak, ze jak konwertujemy SList do Box,
to dla kazdego elementu mamy tak, ze:
- jezeli preceding_whitespace nie zawiera znaku nowej linii,
to przechodzi nam w Space
- w przeciwnym razie, tzn. gdy znak nowej linii sie pojawia sie,
to wszystko przed nim przechodzi nam w 'czopujacy' Space,
ale kolejny element jest juz dodany do nowej linii
(a zeby stworzyc nowa linie, trzeba tez dodac interlinie)

jakie operacje wydaja sie naturalne?

Bit = Atom :following-space Space :contents String
    | Box :following-space Space :first-interline Interline;
Space = Space :width float :following-bit (Bit|null);

Interline = Interline :height float :folowing-line (Line|null);
Line = Line :first-space Space 
            :next-interline Interline;

*/
interface Bit extends Widget {
    Space following_space();
    void set_following_space(Space s);

    void render(Canvas canvas);

    float width();
    float height();

    StringBuilder buildString(StringBuilder sb);

    boolean insertAt(float x, float y,
		     DragAround item);

    //  Bit takeFrom(float x, float y);

    DragAround dragAround(float x, float y,
			  TakeBit take);
    
    Bit shallow_copy();
    Bit deep_copy();
    
}




