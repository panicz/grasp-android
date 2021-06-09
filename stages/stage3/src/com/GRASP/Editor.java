package com.GRASP;
import android.graphics.Canvas;
import android.graphics.RectF;
//import android.graphics.Path;


import java.lang.Math;


class Editor extends Panel {

    Document document;

    Grab transform;

    float scale = 1.0f;
    float angle = 0.0f; // degrees
    
    static int instances = 0;

    int id;
    
    public boolean is_pinned = false;
    
    public Editor(float x, float y, float w, float h,
		  Document doc, Grab grab) {
	super(x, y, w, h);
	document = doc;
	transform = grab;
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
			  document, transform.copy());
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
	transform.scrollBy(x, y);
    }
    
    @Override
    public void render(Canvas canvas) {
	
	canvas.save();

	transform.canvas(canvas);
	
	document.render(canvas);
	canvas.restore();

	float x0_0 = transform.x(5, 5);
	float y0_0 = transform.y(5, 5);
	float x100_0 = transform.x(100, 5);
	float y100_0 = transform.y(100, 5);
	float x0_200 = transform.x(5, 200);
	float y0_200 = transform.y(5, 200);


	canvas.drawLine(x0_0, y0_0, x100_0, y100_0, GRASP.paint);
	canvas.drawLine(x0_0, y0_0, x0_200, y0_200, GRASP.paint);


	canvas.drawLine(transform.unx(x0_0, y0_0),
			transform.uny(x0_0, y0_0),
			transform.unx(x100_0, y100_0),
			transform.uny(x100_0, y100_0),
			GRASP.paint);
	
	canvas.drawLine(transform.unx(x0_0, y0_0),
			transform.uny(x0_0, y0_0),
			transform.unx(x0_200, y0_200),
			transform.uny(x0_200, y0_200), GRASP.paint);

    }
    
    final float [] pending_x = new float[10];
    final float [] pending_y = new float[10];

    final byte [] pending_index = new byte [] {
	-1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1
    };
    
    byte pending = 0;

    boolean [] occupied_index = new boolean[] {
	false, false, false, false, false,
	false, false, false, false, false
    };

    byte occupy_first_free_index(byte finger) {
	for(byte i = 0; i < Screen.fingers; ++i) {
	    if (!occupied_index[i]) {
		occupied_index[i] = true;
		pending_index[finger] = i;
		++pending;
		return i;
	    }
	}
	assert(false);
	return -1;
    }

    void release_index(byte finger) {
	byte index = pending_index[finger];
	assert(index >= 0);
	assert(occupied_index[index]);
	occupied_index[index] = false;
	pending_index[finger] = -1;
	if (--pending == 0) {
	    return;
	}
	for (byte i = 0; i < Screen.fingers; ++i) {
	    byte k = pending_index[i];
	    if (k > index) {
		occupied_index[k] = false;
		occupied_index[k-1] = true;
		pending_x[k-1] = pending_x[k];
		pending_y[k-1] = pending_y[k];

		pending_index[i] = (byte)(k-1);
	    }
	}
    }

    
    class Stretch implements Drag {

	Editor target;
	byte finger;

	float dx = 0;
	float dy = 0;
	
	public Stretch(Editor target, byte finger,
		       float start_x, float start_y) {
	    Panel.stretches++;
	    this.finger = finger;
	    this.target = target;
	    byte index = target.occupy_first_free_index(finger);
	    target.pending_x[index] = start_x;
	    target.pending_y[index] = start_y;
	    transform.anchor(target.pending_x,
			     target.pending_y,
			     target.pending);
	}

	@Override
	public void move(Screen screen, float x, float y,
			 float _dx, float _dy) {
	    byte index = pending_index[finger];
	    target.pending_x[index] = x + dx;
	    target.pending_y[index] = y + dy;
	}

	@Override
	public void drop(Screen screen, float x, float y,
			 float vx, float vy) {
	    target.release_index(finger);
	    Panel.stretches--;
	    transform.anchor(target.pending_x,
			     target.pending_y,
			     target.pending);
	}

	@Override
	public Drag outwards(Transform transform) {
	    float x = transform.unx(dx, dy);
	    float y = transform.uny(dx, dy);
	    dx = x;
	    dy = y;
	    return this;
	}

	@Override
	public Drag inwards(Transform transform) {
	    float x = transform.x(dx, dy);
	    float y = transform.y(dx, dy);
	    dx = x;
	    dy = y;

	    return this;
	}
    }

    @Override
    public Drag stretchFrom(byte finger, float x, float y) {
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

	if (pending == 0) {
	    return;
	}

	transform.towards(pending_x, pending_y, pending);
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
			byte finger,
			float x, float y) {
	if (GRASP.last_known_edit_instance.isOngoingDragAction()) {
	    return new Stretch(this, finger, x, y);
	}

	DragAround drag = document.dragAround(transform.unx(x, y),
					      transform.uny(x, y),
					      takeOriginal);

	if (drag != null) {
	    if (drag.target == document.root) {
		screen.overlay.removeLastOccurrence(drag);
	    }
	    else if (drag != null) {
	    
		return drag.outwards(transform);
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
			byte finger,
			float x, float y) {
	//GRASP.log(toString()+" click");
    }

    
    @Override
    public Drag onSecondPress(Screen screen,
			      byte finger,
			      float x, float y) {
	Drag drag = document.dragAround(transform.unx(x, y),
					transform.uny(x, y),
					takeCopy);

	if (drag != null) {
	    return drag.outwards(transform);
	}


	return null;
    }

    @Override
    public void onDoubleClick(Screen screen,
			      byte finger,
			      float x, float y) {
	//GRASP.log(toString()+" double click");
	transform.reset();
    }

    @Override
    public Drag onHold(Screen screen,
		       byte finger,
		       float x, float y) {
	//GRASP.log(toString()+" hold");
	GRASP._log.clear();
	return null;
    }

    @Override
    public boolean insertAt(float x, float y, DragAround bit) {
	return document.insertAt(transform.unx(x, y),
				 transform.uny(x, y),
				 (DragAround)
				 bit.inwards(transform));
    }
    
}
