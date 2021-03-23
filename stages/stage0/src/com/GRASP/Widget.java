package com.GRASP;

import android.graphics.Canvas;

/*
 * oprocz tego, ze widget mozemy wyswietlac,
 * mozemy rowniez wchodzic z nimi w interakcje.
 * jakie?
 * klasycznie:
 * - muskanie
 * - klikniecie
 * - podwojne klikniecie
 * - przytrzymanie
 * - wcisniecie/puszczenie klawisza
 * - najechanie widgetem nad dany widget (Drag)
 * - upuszczenie widgetu na dany widget (Drop)
 * 
 * Uwagi dotyczace drag&dropa:
 * - gospodarz musi chciec (i wiedziec jak) przyjac goscia
 * - czy gosc musi chciec byc przyjety?
 *
 */


abstract class Widget extends Utils {
    abstract void render(Canvas canvas,
			 float clip_left, float clip_top,
			 float clip_width, float clip_height);
    //Skim skim(float x, float y, int finger);

    abstract float width();
    abstract float height();

    Clipped clippedTo(float w, float h) {
	return new Clipped(w, h, this);
    }

    Displaced displacedBy(float x, float y) {
	return new Displaced(x, y, this);
    }
}


/*
 * main_widget =
 *  Displaced.By(left, top, // X
 *    Clipped.To(width, height // Y
 *      Grouped.Downwards( // Z
 *        Text(A), Text(B), Text(C), Text(D), Text(E))))
 *
 *     	       	       top
 *	    +-----------o------------------+
 *	    |           |  vscroll         |
 *     	    |   +- - - -|- -o- - - - +     |
 *	    |   |       | A |  	     |     |
 *	    |    - - - -|- -|- - - -      s|
 *     	  l |   |    +--v---v-----+\-|    c|
 *     	  e o-------->    B       |h      r|
 *	  f | h |- - +------------+e |    e|
 *	  t | s o---->    C       |i   	  e|
 *	    | c |    |            |g |    n|
 * 	    | r  - - +------------+h      ||
 *	    | o |    |    D       |t |    h|
 *	    | l      Y------------+/-     e|
 *	    | l |    \.	.width.	 ./  |    i|
 *	    |    - - - - - - - - - -      g|
 *	    |   |         E          |    h|
 *	    |                             t|
 *	    |   Z- - - - - - - - - - +     |
 * 	    |         screen-width         |
 * 	    X------------------------------+
 *
 * main: X.render(0,0, screen-width, screen-height)
 * X: translate(left, top)
 *    Y.render(0, 0, 
 *             screen-width - Y.width, 
 *             screen.height - Y.height)
 * Y: translate(-hscroll, -vscroll)
 *    Z.render(hscroll, vscroll, Y.width, Y.height)
 * Z: A: skip
 *    B: translate(0, A.height)
 *       B.render()
 */
