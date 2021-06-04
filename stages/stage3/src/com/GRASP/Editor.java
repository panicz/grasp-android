package com.GRASP;
import android.graphics.Canvas;
import android.graphics.RectF;
//import android.graphics.Path;


import java.lang.Math;


class Editor extends Panel {

    Document document;

    //Transform transform = new Grab();
    
    float horizontal_scroll = 0.0f;
    float vertical_scroll = 0.0f;
    float scale = 1.0f;
    float angle = 0.0f; // degrees
    
    static int instances = 0;

    int id;
    
    public boolean is_pinned = false;
    
    public Editor(float x, float y, float w, float h,
		  Document doc, float hscroll, float vscroll) {
	super(x, y, w, h);
	document = doc;
	horizontal_scroll = hscroll;
	vertical_scroll = vscroll;
	id = instances++;
	// powinnismy zwracac opcje dla dokumentu
	// albo ktoregos jego elementu

    }

    @Override
    public String toString() {
	return String.valueOf(id);
    }
    
    @Override
    public Panel copy() {
	return new Editor(left(), top(), width(), height(),
			  document,
			  horizontal_scroll,
			  vertical_scroll);
    }
    
    @Override
    public boolean canBeSplittedVerticallyBy(RectF line) {
	return top() < line.top
	    && line.bottom < bottom()
	    && line.left - left() < near_edge
	    && right() - line.right < near_edge;
    }

    @Override
    public boolean canBeSplittedHorizontallyBy(RectF line) {
	return left() < line.left
	    && line.right < right() 
	    && line.top - top() < near_edge
	    && bottom() - line.bottom < near_edge;
    }

    @Override
    public Panel splitHorizontallyBy(RectF line) {
	return new HorizontalSplit(this, line);
    }

    @Override
    public Panel splitVerticallyBy(RectF line) {
	return new VerticalSplit(this, line);
    }

    @Override
    public void scrollBy(float x, float y) {
	horizontal_scroll += x;
	vertical_scroll += y;
    }
    
    @Override
    public void render(Canvas canvas) {
	
	canvas.save();

	canvas.scale(scale, scale);
	canvas.rotate(angle);

	
	canvas.translate(horizontal_scroll,
			 vertical_scroll);
	
	
	document.render(canvas);
	canvas.restore();
    }


    final float [] pin_x = new float[10];
    final float [] pin_y = new float[10];
    
    final float [] stretch_x = new float[10];
    final float [] stretch_y = new float[10];

    final float [] shift_x = new float[10];
    final float [] shift_y = new float[10];

    final byte [] pending_index = new byte[10];

    
    boolean[] stretching = new boolean[] {
	false, false, false, false, false,
	false, false, false, false, false
    };

    class Stretch implements Drag {

	Editor target;
	int finger;
	
	public Stretch(Editor target, int finger,
		       float start_x, float start_y) {

	    Panel.stretches++;
	    this.finger = finger;
	    this.target = target;
	    target.stretching[finger] = true;
	    target.pin_x[finger] = start_x;
	    target.pin_y[finger] = start_y;

	    target.shift_x[finger] = 0;
	    target.shift_y[finger] = 0;
	}

	@Override
	public void move(Screen screen, float x, float y,
			 float dx, float dy) {
	    target.stretch_x[finger] = x;
	    target.stretch_y[finger] = y;
	}

	@Override
	public void drop(Screen screen, float x, float y,
			 float vx, float vy) {
	    target.stretching[finger] = false;
	    Panel.stretches--;
	}

	@Override
	public Drag translate(float x, float y) {
	    target.pin_x[finger] += x;
	    target.pin_y[finger] += y;
	    return this;
	}
    }

    @Override
    public Drag stretchFrom(int finger, float x, float y) {
	return new Stretch(this, finger, x, y);
    }

    @Override
    public void stretch() {
	/*
	 * to dziala tak: ruch powoduje zmiane wartosci stretch_
	 * (w funkcji move), a pozniej jest wywolywana funkcja
	 * 'stretch'.
	 * w tej wlasnie funkcji powinnismy ustawiac wartosci
	 * skali, przesuniecia i ewentualnie obrotu.
	 *
	 * pin reprezentuje stara pozycje palca, a  stretch
	 * - nowa pozycje.
	 */
	byte pending = 0;
	for (byte i = 0; i < Screen.fingers; ++i) {
	    if (stretching[i]) {
		pending_index[pending++] = i;
	    }
	}

	if (pending == 0) {
	    return;
	}

	if (pending == 1) {
	    byte i = pending_index[0];
	    scrollBy(shift_x[i]/scale, shift_y[i]/scale);
	    shift_x[i] = stretch_x[i] - pin_x[i];
	    shift_y[i] = stretch_y[i] - pin_y[i];
		
	    pin_x[i] = stretch_x[i];
	    pin_y[i] = stretch_y[i];
	    return;
	}

	if (pending > 2) {
	    // docelowo moglibysmyzrobic tu obracanie
	    pending = 2;
	}


	float px = pin_x[0]-pin_x[1];
	float py = pin_y[0]-pin_y[1];

	float d1 = (float) Math.sqrt(S.qr(px) + S.qr(py));

	float sx = stretch_x[0]-stretch_x[1];
	float sy = stretch_y[0]-stretch_y[1];
	
	float d2 = (float) Math.sqrt(S.qr(sx) + S.qr(sy));

	float new_scale = scale*d2/d1;

	float da = (float)(Math.atan2(sy,sx) - Math.atan2(py,px));
	
	float a0 = (float) Math.toRadians(angle);
	float a1 = a0 + da;
	
	float sin_a0 = (float) Math.sin(a0);
	float cos_a0 = (float) Math.cos(a0);

	float sin_a1 = (float) Math.sin(a1);
	float cos_a1 = (float) Math.cos(a1);

	float dx = (cos_a0*pin_x[0] + sin_a0*pin_y[0])/scale
	    - (cos_a1*stretch_x[0] + sin_a1*stretch_y[0])/new_scale;
	float dy = (-sin_a0*pin_x[0] + cos_a0*pin_y[0])/scale
	    - (-sin_a1*stretch_x[0] + cos_a1*stretch_y[0])/new_scale;
	
	scrollBy(-dx, -dy);
	scale = new_scale;
	angle = (float) Math.toDegrees(a1);

	for (byte n = 0; n < pending; ++n) {
	    byte i = pending_index[n];
	    pin_x[i] = stretch_x[i];
	    pin_y[i] = stretch_y[i];
	}
	
    }

    float docx(float scrx) {
	return (scrx-horizontal_scroll)/scale;
    }
    float docy(float scry) {
	return (scry-vertical_scroll)/scale;
    }

    float scrx(float docx) {
	return docx/scale + horizontal_scroll;
    }

    float scry(float docy) {
	return docy/scale * vertical_scroll;
    }
    
    class TakeOriginal implements TakeBit {
	@Override
	public Bit from(Space space) {
	    return space.remove_following_bit();	    
	}
    }

    TakeBit takeOriginal = new TakeOriginal();

    class TakeCopy implements TakeBit {
	@Override
	public Bit from(Space space) {
	    Bit copy = space.following_bit.shallow_copy();
	    copy.set_following_space(null);
	    return copy.deep_copy();
	}
    }

    TakeBit takeCopy = new TakeCopy();
    
    @Override
    public Drag onPress(Screen screen,
			int finger,
			float x, float y) {
	if (GRASP.last_known_edit_instance.isOngoingDragAction()) {
	    return new Stretch(this, finger, x, y);
	}

	DragAround drag = document.dragAround(docx(x),
					docy(y),
					takeOriginal);

	if (drag != null) {
	    if (drag.target == document.root) {
		screen.overlay.removeLastOccurrence(drag);
	    }
	    else {
	    
	    return drag;
	    /*
	    return translate(drag,
			     horizontal_scroll*scale,
			     vertical_scroll*scale);*/
	    }
	}


	return null;
	
	
	//return null;	
	/*
	Location source = document
	    .locationOfElementAtPosition
	    (x + horizontal_scroll,
	     y + vertical_scroll);
	if (source == null) {
	    return null;
	}

	Element target = document
	    .takeElementFromLocation(source);

	return new MoveAround(screen.add(target));
	*/
    }

    @Override    
    public void onClick(Screen screen,
			int finger,
			float x, float y) {
	//GRASP.log(toString()+" click");
    }

    
    @Override
    public Drag onSecondPress(Screen screen,
			      int finger,
			      float x, float y) {
	Drag drag = document.dragAround(docx(x),
					docy(y),
					takeCopy);

	if (drag != null) {
	    return translate(drag,
			     horizontal_scroll,
			     vertical_scroll);
	}


	return null;
    }

    @Override
    public void onDoubleClick(Screen screen,
			      int finger,
			      float x, float y) {
	//GRASP.log(toString()+" double click");

    }

    @Override
    public Drag onHold(Screen screen,
		       int finger,
		       float x, float y) {
	//GRASP.log(toString()+" hold");
	return null;
    }

    @Override
    public boolean insertAt(float x, float y, DragAround bit) {
	return document.insertAt(docx(x),
				 docy(y),
				 (DragAround)
				 translate(bit,
					   -horizontal_scroll,
					   -vertical_scroll));
    }
    
}
