package com.GRASP;
import android.graphics.Canvas;
import android.graphics.RectF;
//import android.graphics.Path;
import java.util.List;
import java.util.WeakHashMap;
import android.os.Parcel;
import android.os.Parcelable;
import java.util.Iterator;
import android.os.Environment;
import java.io.File;
import android.net.Uri;

import java.lang.Math;

final class Editor extends Panel {

    Document document;

    Grab transform;

    Transition transition;
    
    float scale = 1.0f;
    float angle = 0.0f; // degrees
    
    public boolean is_pinned = false;

    public Editor(float x, float y, float w, float h,
		  Document doc, Grab grab) {
	super(x, y, w, h);
	document = doc;
	transform = grab;
	transition = new Transition(this);
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

	GRASP.paint.setTypeface(GRASP.logs_font);
	GRASP.paint.setTextSize(16);
	canvas.drawText(document.file.getPath(), 0, height()-2,
			GRASP.paint);

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

    WeakHashMap<Document, Grab> documentTransform =
	new WeakHashMap<Document, Grab>();

    WeakHashMap<Document, Document> previousDocument =
	new WeakHashMap<Document, Document>();

    
    void switchToDocument(Document target) {
	Grab grab = documentTransform.get(target);
	if (grab == null) {
	    grab = new Grab();
	}
	documentTransform.put(document, transform);
	document = target;
	transform = grab;
    }
    
    @Override
    public boolean closeDocument(Document document) {
	if (this.document != document) {
	    return true;
	}

	Document replacement = previousDocument.get(document);
	if (replacement == null
	    || replacement == document) {
	    Iterator<Document> it = Document
		.openedDocuments.iterator();

	    while(it.hasNext()) {
		replacement = it.next();

		if (replacement != document) {
		    switchToDocument(replacement);
		    return true;
		}
	    }
	    return false;
	}

	switchToDocument(replacement);
	return true;
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
	    (new
	     Popup(new
		   Below(new Button("New", new
				    CreateNewDocument(screen, this)),
			 new
			 Button("Open...", new
				OpenFileBrowser(screen, this,
						Environment
						.getExternalStorageDirectory())),
			 new Button("Switch to...", new
				    ShowOpenedDocuments(screen,
							this)),
			 new Button("Save"),
			 new Button("Save as...", new
				    SaveFileBrowser(screen, this,
						    document.file.getParentFile())),
			 new Button("Close", new
				    CloseDocument(screen,
						  document))
			 ))).centerAround(screen.x[finger],
					  screen.y[finger],
					  screen.width,
					  screen.height);
    }

    @Override
    public boolean insertAt(float x, float y, DragAround bit) {
	return document.insertAt(transform.unx(x, y),
				 transform.uny(x, y),
				 (DragAround)
				 bit.inwards(transform));
    }

    @Override
    public void writeDataToParcel(Parcel out, int flags) {
	out.writeFloat(_left);
	out.writeFloat(_top);
	out.writeFloat(_width);
	out.writeFloat(_height);
	out.writeString(document.file.getPath());
	out.writeFloat(transform.getLeft());
	out.writeFloat(transform.getTop());
	out.writeFloat(transform.getScale());
	out.writeFloat(transform.getAngle());
    }

    public static Editor fromParcel(Parcel in) {
	float x = in.readFloat();
	float y = in.readFloat();
	float w = in.readFloat();
	float h = in.readFloat();
	String path = in.readString();
	float left = in.readFloat();
	float top = in.readFloat();
	float scale = in.readFloat();
	float angle = in.readFloat();
	return new Editor(x, y, w, h,
			  Document.fromFile(new File(path)),
			  new Grab(left, top, scale, angle));

    }
}
