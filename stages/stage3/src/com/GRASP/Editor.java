package com.GRASP;
import android.graphics.Canvas;
import android.graphics.RectF;
//import java.lang.Math;


class Editor extends Interactions {

    Document document;

    float horizontal_scroll = 0.0f;
    float vertical_scroll = 0.0f;

    String id;

    static int instances = 0;

    PopUp operations;
    
    @Override
    public String toString() {
	return id;
    }

    public boolean is_pinned = false;
    
    
    public Editor(float x, float y, float w, float h,
		  Document doc, float hscroll, float vscroll) {
	super(x, y, w, h);
	document = doc;
	horizontal_scroll = hscroll;
	vertical_scroll = vscroll;
	id = String.valueOf(++instances);
	
	// powinnismy zwracac opcje dla dokumentu
	// albo ktoregos jego elementu

    }
    
    @Override
    public Interactions copy() {
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
    public Interactions splitHorizontallyBy(RectF line) {
	return new HorizontalSplit(this, line);
    }

    @Override
    public Interactions splitVerticallyBy(RectF line) {
	return new VerticalSplit(this, line);
    }

    @Override
    public void scrollBy(float x, float y) {
	horizontal_scroll += x;
	vertical_scroll += y;
    }
    
    @Override
    public void render(Canvas canvas) {
	String pos = "("+(int)left()+", "+(int)top()+")";
	GRASP.paint.setTextSize(18);
	canvas.drawText(pos,
			left() + width()/2.0f - 6*pos.length(),
			top() + height()/2.0f - 36,
			GRASP.paint);
	
	GRASP.paint.setTextSize(36);
	canvas.drawText(id,
			left() + width()/2.0f,
			top() + height()/2.0f,
			GRASP.paint);

	String size = "("+(int)width()+", "+(int)height()+")";
	GRASP.paint.setTextSize(18);
	canvas.drawText(size,
			left() + width()/2.0f,
			top() + height()/2.0f + 36,
			GRASP.paint);
    }

    class PinDocument implements Action {
	Editor target;
	public PinDocument(Editor target) {
	    this.target = target;
	}
	
	@Override
	public PopUp perform() {
	    target.is_pinned = true;
	    return null;
	}
    };

    class UnpinDocument implements Action {
	Editor target;
	public UnpinDocument(Editor target) {
	    this.target = target;
	}
	
	@Override
	public PopUp perform() {
	    target.is_pinned = false;
	    return null;
	}
    };

    
    class SwitchDocument implements Action {
	Editor target;
	public SwitchDocument(Editor target) {
	    this.target = target;
	}
	
	@Override
	public PopUp perform() {
	    // powinnnismy zwrocic PopUp
	    // ze wszystkimi otwartymi dokumentami
	    return null;
	}
    };

    class OpenDocument implements Action {
	Editor target;
	public OpenDocument(Editor target) {
	    this.target = target;
	}
	
	@Override
	public PopUp perform() {
	    return null;	    
	}
    };

    
    class SaveDocument implements Action {
	Editor target;
	public SaveDocument(Editor target) {
	    this.target = target;
	}
	
	@Override
	public PopUp perform() {
	    return null;	    
	}
    };

    class SaveDocumentAs implements Action {
	Editor target;
	public SaveDocumentAs(Editor target) {
	    this.target = target;
	}
	
	@Override
	public PopUp perform() {
	    // powinnnismy zwrocic PopUp
	    // z wyborem sciezki
	    // i polem tekstowym
	    return null;
	}
    };

    
    @Override
    public PopUp choices(float x, float y) {
		
	//operations = new PopUp(x, y, 280, 300);
	// Pin Document
	// Switch to
	// Open
	// Save
	// Save as
	// 	
	
	return new
	    Choices(is_pinned
		    ? new Button("Unpin",
				 new UnpinDocument(this))
		    : new Button("Pin",
				 new PinDocument(this)),
		    new Button("Switch to",
			       new SwitchDocument(this)),
		    new Button("Open",
			       new OpenDocument(this)),
		    new Button("Save",
			       new SaveDocument(this)),
		    new Button("Save as",
			       new SaveDocumentAs(this)));
    }

}
