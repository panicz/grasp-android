package com.GRASP;
import android.graphics.Canvas;
import android.graphics.RectF;
//import android.graphics.Path;


import java.lang.Math;


class Editor extends Panel {

    Document document;

    Grab transform;

    Animation animation;

    Screen screen;
    
    float scale = 1.0f;
    float angle = 0.0f; // degrees
    
    static int instances = 0;

    int id;
    
    public boolean is_pinned = false;

    public Editor(Screen owner,
		  float x, float y, float w, float h,
		  Document doc, Grab grab) {
	super(x, y, w, h);
	screen = owner;
	document = doc;
	transform = grab;
	id = instances++;
	animation = new Animation(this);
	// powinnismy zwracac opcje dla dokumentu
	// albo ktoregos jego elementu
    }

    @Override
    public String toString() {
	return String.valueOf(id);
    }
    
    @Override
    public Panel copy() {
	return new Editor(screen, left(), top(), width(), height(),
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
	
	transform.canvas(canvas);
	document.render(canvas);
	transform.uncanvas(canvas);
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
	if (animation.is_running()) {
	    animation.stop();
	}
	
	if (GRASP.last_known_edit_instance.isOngoingDragAction()) {
	    return new Stretch(this, finger, x, y);
	}

	Drag drag = document.dragAround(transform.unx(x, y),
					transform.uny(x, y),
					takeOriginal);

	if (drag != null) {
	    if (drag instanceof DragAround) {
		screen.overlay.push((DragAround)drag);
	    }
	    return drag.outwards(transform);
	}
	

	return null;
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
	    if (drag instanceof DragAround) {
		screen.overlay.push((DragAround)drag);
	    }

	    return drag.outwards(transform);
	}

	return stretchFrom(finger, x, y);
    }

    @Override
    public void onDoubleClick(Screen screen,
			      byte finger,
			      float x, float y) {
	// jak to powinno dzialac:
	// 1. jezeli klikniemy na wyrazeniu, to chcemy
	// je zmaksymalizowac, tzn dopasowac do szerokosci
	// albo wysokosci edytora i wypozycjonowac mniej wiecej
	// na srodku ekranu

	// jezeli wyrazenie jest zmaksymalizowane, to
	// podwojne klikniecie powinno ustawic taka skale,
	// zeby widac bylo caly dokument

	// tak samo powinno byc w przypadku klikniecia
	// poza wyrazeniem - tzn powinnismy tak ustawic skale,
	// zeby bylo widac caly dokument
	
	if (Math.abs(transform.getAngle()) > 0.1) {
	    animation.setTargetAngle(0.0f);
	    animation.setTargetScale(transform.getScale());
	    animation.fixPoint(x, y);
	    animation.start((int) Math.abs(720*transform.getAngle()
					   /90));
	    return;
	}

	animation.setTargetAngle(transform.getAngle());

	float W = width();
	float H = height();
	
	float x_ = transform.unx(x, y);
	float y_ = transform.uny(x, y);
	
	DragAround target = document.topLevelItemAt(x_, y_);

	if (target != null) {
	    float w = 2*Document.min_space_between_bits
		+ target.width();
	    float h = 2*Document.min_space_between_bits
		+ target.height();
	    float scale = (float)
		//Math.min(1.0,
			 Math.min(H/h,
				  W/w);
	    float left = target.x
		- Document.min_space_between_bits;
	    
	    float top = target.y - (H/scale-h)/2;

	    if (Math.abs(left + transform.getLeft()) > 10
		|| Math.abs(top + transform.getTop()) > 10
		|| Math.abs(scale - transform.getScale()) > 0.01) {
		//GRASP.log("focus on "+target.target);
		animation.setTargetScale(scale);
		animation.setScroll(-left, -top);
		animation.start(700);
		return;
	    }
	}
	

	float height_ratio = height()/document.height();

	float width_ratio = width()/document.width();
	
	if (Math.abs(height_ratio - transform.getScale()) < 0.01) {

	    animation.setTargetScale(width_ratio);
	    animation.fixPoint(0, y);
	    animation.start(700);
	    return;
	}


	animation.setTargetScale(height_ratio);
	animation.setScroll(0, 0);
	animation.start(700);

	//GRASP.log(toString()+" double click");
	/*
	if(transform.getAngle() != 0) {
	    animation.setTargetAngle(0, x, y);
	    animation.start(1000);
	    //return;
	}

	float whole_document = document.height()/height();
	    
	if (transform.getScale() != whole_document) {
	    animation.setTargetScale(whole_document);
	    animation.start(1000);
	    //return;
	}
	*/
	//animation.setTargetTransform(0.0f, 0.0f, 1.0f, 0.0f);
	
	//transform.reset();
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
