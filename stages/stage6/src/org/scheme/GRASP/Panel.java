package org.scheme.GRASP;
//import android.graphics.Canvas;
import android.graphics.RectF;
import android.os.Parcelable;
import android.os.Parcel;


abstract class Panel implements Pad, Parcelable {
    static final float near_edge = 60;

    static int stretches = 0;
    
    protected float _left;
    protected float _top;
    protected float _width;
    protected float _height;
    protected float _right;
    protected float _bottom;

    //@Deprecated
    public float left() { return _left; }
    
    //@Deprecated
    public float top() { return _top; }
    
    @Override
    public float width() {
	return _width;
    }
    
    @Override
    public float height() {
	return _height;
    }

    //@Deprecated
    public float right() { return _right; }
    
    //@Deprecated
    public float bottom() { return _bottom; }

    public void setLeft(float v) {
	_left = v;
	_right = _left + _width;
    }

    public void setTop(float v) {
	_top = v;
	_bottom = _top + _height;
    }

    public void setWidth(float v) {
	_width = v;
	_right = _left + _width;
    }

    public void setHeight(float v) {
	_height = v;
	_bottom = _top + _height;
    }    

    @Override
    public void trySetSize(float w, float h) {
	setWidth(w);
	setHeight(h);
    }
    
    public Panel(float x, float y, float w, float h) {
	_left = x;
	_top = y;
	_width = w;
	_height = h;
	_right = x+w;
	_bottom = y+h;
    }

    public abstract boolean closeDocument(Document document);
    
    public abstract Panel copy();
    
    public abstract boolean
	canBeSplittedVerticallyBy(RectF line);

    public abstract boolean
	canBeSplittedHorizontallyBy(RectF line);

    @Override
    public Drag onPress(Screen screen,
			byte finger,
			float x, float y) {
	return null;
    }

    @Override
    public void onClick(Screen screen,
			byte finger,
			float x, float y) {}

    @Override
    public Drag onSecondPress(Screen screen,
			      byte finger,
			      float x, float y) {
	return null;
    }

    @Override
    public void onDoubleClick(Screen screen,
			      byte finger,
			      float x, float y) {
    }

    @Override
    public Drag onHold(Screen screen,
		       byte finger,
		       float x, float y) {
	return null;
    }
    
    public abstract void scrollBy(float x, float y);
    
    public abstract Panel
	splitHorizontallyBy(RectF line);

    public abstract Panel
	splitVerticallyBy(RectF line);
    
    public Panel
	finishResizing(Split s, float vx, float vy) {
	return this;
    }

    public abstract void onDelete();
    
    public Panel at(float x, float y) {
	return this;
    }

    public Panel at(Point p) {
	return at(p.x, p.y);
    }
    
    public abstract Drag stretchFrom(byte finger, float x, float y);
    
    public abstract void stretch();

    public abstract Space insertAt(float x, float y,
				   DragAround bit,
				   Ref<Line> line);

    public abstract void insertAt(float x, float y,
				  DragAround bit);

    
    protected static final Shift shift = new Shift();
    
    protected static Drag translate(Drag drag, float x, float y) {
	if (drag == null) {
	    return null;
	}
		
	shift.set(x, y);
	return drag.outwards(shift);
    }

    @Override
    public void onDragOver(Screen screen, byte finger,
			   float x, float y) {}

    @Override
    public void onDragOut(Screen screen, byte finger) {}

    @Override
    public void onRelease(Screen screen, byte finger,
			  float x, float y) {}

    @Override
    public void onRemove(Screen screen) {}
    
    static final byte PANEL_TYPE_EDITOR = 0;
    static final byte PANEL_TYPE_HORIZONTAL_SPLIT = 1;
    static final byte PANEL_TYPE_VERTICAL_SPLIT = 2;
    
    @Override
    public int describeContents() {
	// CONTENTS_FILE_DESCRIPTOR?
	return 0;
    }

    abstract void writeDataToParcel(Parcel out, int flags);
    
    @Override
    public void writeToParcel(Parcel out, int flags) {
	if (this instanceof Editor) {
	    out.writeByte(PANEL_TYPE_EDITOR);
	}
	else if (this instanceof HorizontalSplit) {
	    out.writeByte(PANEL_TYPE_HORIZONTAL_SPLIT);
	}
	else if (this instanceof VerticalSplit) {
 	    out.writeByte(PANEL_TYPE_VERTICAL_SPLIT);
	}
	else {
	    assert(false);
	}
	
	writeDataToParcel(out, flags);
    }
    
    public static final Parcelable.Creator<Panel> CREATOR =
	new Parcelable.Creator<Panel>() {
	    public Panel createFromParcel(Parcel in) {
		
		byte parcelTag = in.readByte();
		switch (parcelTag) {
		default:
		case PANEL_TYPE_EDITOR:
		    return Editor.fromParcel(in);
		case PANEL_TYPE_HORIZONTAL_SPLIT:
		    return HorizontalSplit.fromParcel(in);
		case PANEL_TYPE_VERTICAL_SPLIT:
		    return VerticalSplit.fromParcel(in);
		}
	    }

	    public Panel[] newArray(int size) {
		return new Panel[size];
	    }
	};

    
    
};
