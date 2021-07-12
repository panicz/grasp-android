package com.GRASP;
import android.graphics.Canvas;
import android.graphics.RectF;
//import android.graphics.Path;
import java.util.List;
import android.os.Parcel;
import android.os.Parcelable;

import java.lang.Math;


final class Editor extends Panel {

    Document document;

    Grab transform;

    Transition transition;
    
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
	transition = new Transition(this);
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
	if (transition.is_running(screen.animationSystem)) {
	    transition.stop(screen.animationSystem);
	}
	
	if (screen.isOngoingDragAction()) {
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
	// docelowo bedziemy chcieli umiescic kursor
	// na danym wyrazeniu
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
	if (Math.abs(transform.getAngle()) > 0.1) {
	    transition.setTargetAngle(0.0f);
	    transition.setTargetScale(transform.getScale());
	    transition.fixPoint(x, y);
	    transition.start((int) Math.abs(720*transform.getAngle()
					    /90),
			     screen.animationSystem);
	    return;
	}

	transition.setTargetAngle(transform.getAngle());

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
	    float scale = (float) Math.min(H/h, W/w);
	    float left = target.x
		- Document.min_space_between_bits;
	    
	    float top = target.y - (H/scale-h)/2;

	    if (Math.abs(left + transform.getLeft()) > 10
		|| Math.abs(top + transform.getTop()) > 10
		|| Math.abs(scale - transform.getScale()) > 0.01) {
		//GRASP.log("focus on "+target.target);
		transition.setTargetScale(scale);
		transition.setScroll(-left, -top);
		transition.start(700, screen.animationSystem);
		return;
	    }
	}
	

	float height_ratio = height()/document.height();

	float width_ratio = width()/document.width();
	
	if (Math.abs(height_ratio - transform.getScale()) < 0.01) {

	    transition.setTargetScale(width_ratio);
	    transition.fixPoint(x, y);
	    transition.start(700, screen.animationSystem);
	    return;
	}


	transition.setTargetScale(height_ratio);
	transition.setScroll(0, 0);
	transition.start(700, screen.animationSystem);
    }
    
    class ShowOpenedDocuments implements Action {
	Screen target;
	
	public ShowOpenedDocuments(Screen screen) {
	    target = screen;
	}

	@Override
	public void perform(byte finger, float x, float y) {
	    x = target.x[finger];
	    y = target.y[finger];
	    target.layers.removeLast();
	    List<Document> opened = Document.openedDocuments;
	    Button [] documents = new Button[opened.size()];
	    
	    for (int i = 0; i < opened.size(); ++i) {
		Document doc = opened.get(i);
		documents[i] = new Button(doc.path);
	    }

	    Popup popup = new Popup(new Below(documents));
	    popup.centerAround(x, y,
			       target.width,
			       target.height);

	    target.layers.addLast(popup);
	}
    }
    
    @Override
    public Drag onHold(Screen screen,
		       byte finger,
		       float x, float y) {
	//GRASP.log(toString()+" hold");
	GRASP._log.clear();

	DragAround item =
	    document.topLevelItemAt(transform.unx(x, y),
				    transform.uny(x, y));

	if (item != null) {
	    // ...
	    // opcje dla tego czegos co zlapalismy
	    GRASP.log(item.target.toString());
	    return null;
	}
	
	return
	    new
	    Popup(new
		  Below(new Button("New"),
			new Button("Open"),
			new Button("Switch to...",
				   new ShowOpenedDocuments(screen)),
			new Button("Save"),
			new Button("Save as..."),
			new Button("Close")
			));
    }

    @Override
    public boolean insertAt(float x, float y, DragAround bit) {
	return document.insertAt(transform.unx(x, y),
				 transform.uny(x, y),
				 (DragAround)
				 bit.inwards(transform));
    }

    @Override
    public void writeToParcel(Parcel out, int flags) {
	out.writeByte(PANEL_TYPE_EDITOR);

    }

    public static Editor fromParcel(Parcel in) {
	// the PANEL_TYPE_EDITOR parcel tag has already been
	// read by Panel's Parcelable.Creator
	return null;
    }
}
