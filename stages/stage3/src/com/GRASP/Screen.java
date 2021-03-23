package com.GRASP;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.RectF;
import android.util.DisplayMetrics;
import android.view.MotionEvent;
import android.view.View;
import java.util.List;
import java.util.ArrayList;
//import java.util.Deque;
//import java.util.ArrayDeque;
import java.util.Iterator;

import android.view.inputmethod.InputMethodManager;

class Screen extends View {

    public Interactions view;

    GRASP activity;
    
    public float width;
    public float height;

    static final int fingers = 10;
    
    boolean[] finger = new boolean[] {
	false, false, false, false, false,
	false, false, false, false, false
    };
    
    float[] x = new float[10];
    float[] y = new float[10];

    public List<Shape> segments = new ArrayList<Shape>();
    public Shape shape = null;
    public RectF shape_area = new
	RectF(Float.POSITIVE_INFINITY,
	      Float.POSITIVE_INFINITY,
	      Float.NEGATIVE_INFINITY,
	      Float.NEGATIVE_INFINITY);

    public PopUp popup = null;
    public Recognizer recognize;

    class ClearCurrentSymbolAction implements Action {
	Screen screen;
	public ClearCurrentSymbolAction(Screen screen) {
	    this.screen = screen;
	}

	public PopUp perform() {
	    screen.cancelDrawingShape();
	    return null;
	}
    }

    class AddExistingSymbolAction implements Action {
	Screen screen;
	public AddExistingSymbolAction(Screen screen) {
	    this.screen = screen;
	}

	public PopUp perform() {
	    List<String> existing_symbols =
		screen.recognize.known_symbols();

	    List<Button> buttons =
		new ArrayList<Button>();
	
	    Iterator<String> it =
		existing_symbols.iterator();

	    while (it.hasNext()) {
		String symbol = it.next();
		buttons
		    .add(new
			 Button(symbol,
				recognize
				.store(symbol,
				       segments)));
	    }
	    return new Choices(buttons);
	}
    }

    class CreateNewSymbolAction implements Action {
	public CreateNewSymbolAction(Screen screen) {

	}

	public PopUp perform() {
	    return null;
	}
    }
    
    void suggestShapeActions(float x, float y) {
	List<Recognizer.Rank> candidates =
	    recognize.candidates(segments);

	List<Button> buttons = new ArrayList<Button>();
	
	Iterator<Recognizer.Rank> it =
	    candidates.iterator();

	while (it.hasNext()) {
	    Recognizer.Rank rank = it.next();
	    buttons.add(new
			Button(rank.name,
			       new SymbolAction(rank
						.name,
						this)));
	}

	buttons
	    .add(new
		 Button("Clear",
			new
			ClearCurrentSymbolAction(this)));
	buttons
	    .add(new
		 Button("Existing symbol",
			new
			AddExistingSymbolAction(this)));
	buttons
	    .add(new
		 Button("New symbol",
			new
			CreateNewSymbolAction(this)));
	popup = new Choices(buttons);

	setReasonableLocation(popup, x, y);
    }
    
    void startDrawingShape() {
	shape = new Shape();
    }

    void cancelDrawingShape() {
	shape = null;
	segments.clear();
	shape_area.left = Float.POSITIVE_INFINITY;
	shape_area.top = Float.POSITIVE_INFINITY;
	shape_area.right = Float.NEGATIVE_INFINITY;
	shape_area.bottom = Float.NEGATIVE_INFINITY;
    }

    boolean isShapeBeingDrawn() {
	return shape != null;
    }

    void finalizeShapeSegment() {
	if (shape != null
	    && (shape.rect.width() > 4
		|| shape.rect.height() > 4)) {
	    segments.add(shape);
	    if (shape.rect.left < shape_area.left) {
		shape_area.left = shape.rect.left;
	    }
	    if (shape.rect.right > shape_area.right) {
		shape_area.right = shape.rect.right;
	    }
	    if (shape.rect.top < shape_area.top) {
		shape_area.top = shape.rect.top;
	    }
	    if (shape.rect.bottom > shape_area.bottom) {
		shape_area.bottom = shape.rect.bottom;
	    }
	}
	
	shape = null;
    }
    
    public Screen(GRASP source) {
	super(source);

	activity = source;
	recognize = new Recognizer(activity);
	DisplayMetrics metrics =
	    source
	    .getResources()
	    .getDisplayMetrics();

	width = (float) metrics.widthPixels;
	height = (float) metrics.heightPixels;

	view = new Editor(0, 0, width, height,
			  null, 0, 0);
    }

    Skim [] skim = new Skim[] {
	null, null, null, null, null,
	null, null, null, null, null
    };
    
    boolean splittedView(RectF rect) {
	/**
	 * a horizontal line makes a VerticalSplit,
	 * while a vertical line makes a HorizontalSplit
	 */
	if (shape.isHorizontalLine()
	    && view.canBeSplittedVerticallyBy(rect)) {
	    view = view.splitVerticallyBy(rect);
	    //GRASP.log(view.toString());
	    cancelDrawingShape();
	    return true;
	}
	
	if(shape.isVerticalLine()
	   && view.canBeSplittedHorizontallyBy(rect)) {
	    view = view.splitHorizontallyBy(rect);
	    //GRASP.log(view.toString());
	    cancelDrawingShape();
	    return true;
	}

	return false;
    }

    
    public boolean onDown(MotionEvent event) {
	int i = event.getActionIndex();
	int p = event.getPointerId(i);
	int n = event.getPointerCount();
	assert(!finger[p]);
	finger[p] = true;

	x[p] = event.getX(i);
	y[p] = event.getY(i);
	
	if (p > 0 && isShapeBeingDrawn()) {
	    cancelDrawingShape();
	}

	if (popup != null) {
	    if (popup.area(x[p], y[p])
		== PopUp.Area.Outside) {
		popup = null;
	    }
	    else {
		skim[p] = popup.skim(x[p], y[p],
				     width, height);
		return true;
	    }
	}

	Split split = view.splitUnder(x[p], y[p]);
	if (split != null) {
	    skim[p] = split;
	    cancelDrawingShape();
	}
	else if (n == 1 && p == 0
		 && !isShapeBeingDrawn()) {
	    startDrawingShape();
	}
	
	return true;
    }

    public boolean onMotion(MotionEvent event) {

	int n = event.getPointerCount();
	int max_finger = -1;
	    
	for (int i = 0; i < n; ++i) {
	    int p = event.getPointerId(i);
	    float xp = event.getX(i);
	    float yp = event.getY(i);
	    assert(finger[p]);
	    max_finger = (p > max_finger)
		? p : max_finger;

	    if (skim[p] != null) {
		skim[p].through(xp, yp, xp-x[p], yp-y[p]);
	    }

	    x[p] = xp;
	    y[p] = yp;
	}
	
	if (max_finger == 0
	    && n == 1
	    && isShapeBeingDrawn()) {
	    shape.add(x[0], y[0]);
	    GRASP._log.update("("+x[0]+", "+y[0]+")");
	}
	return true;
    }
    
    public boolean onUp(MotionEvent event,
			float vx, float vy) {
	int i = event.getActionIndex();
	int p = event.getPointerId(i);
	assert(finger[p]);
	finger[p] = false;

	if (skim[p] != null) {
	    popup = skim[p].to(this, x[p], y[p], vx, vy);
	    skim[p] = null;
	    return true;
	}
	
	if (isShapeBeingDrawn() && p == 0) {
	    
	    if (segments.isEmpty()
		&& splittedView(shape.rect)) {	       
		return true;
	    }
	    
	    finalizeShapeSegment();
	    suggestShapeActions(x[p], y[p]);
	}
	
	return true;
    }
    
    public boolean onDoubleTap(MotionEvent e) {
	/* co sie dzieje przy podwojnym kliknieciu? */
	// jezeli jest na wyrazeniu, wyrazenie zostaje
	// zmaksymalizowane i przypiete do ekranu
	// (chyba ze juz bylo przypiete -- w takim razie
	// przywracamy kamere sprzed przypiecia)
        return true;
    }

    public boolean onSingleTap(MotionEvent e) {
	/* co sie dzieje przy kliknieciu? */
	float x = e.getX();
	float y = e.getY();
	if (popup != null) {
	    popup = popup.onClick(x, y);
	    return true;
	}
	return false;
    }

    void setReasonableLocation(PopUp choice,
			       float x, float y) {
	if (segments.isEmpty()) {
	    choice.top = y - Button.height/2
		- PopUp.radius - PopUp.margin;
	    choice.left = x - choice.width + 70;
	}
	else {
	    choice.left =
		(shape_area.right - choice.width)/2;
	    if (shape_area.bottom < height/2) {
		choice.top = shape_area.bottom + 70;
	    }
	    else {
		choice.top = shape_area.top
		    - choice.height - 70;
	    }
	    
	}
	if (choice.left+choice.width > width) {
	    choice.left -= choice.left+choice.width
		- width;
	}
	if (choice.top+choice.height > height) {
	    choice.top -= choice.top+choice.height
		- height;
	}
	if (choice.left < 0) {
	    choice.left = 0;
	}
	if (choice.top < 0) {
	    choice.top = 0;
	}
    }
    
    public boolean onLongPress(MotionEvent event) {
	/* co sie dzieje przy przytrzymaniu?*/
	float x = event.getX();
	float y = event.getY();

	cancelDrawingShape();

	if (popup == null) {
	    popup = view.choices(x, y);
	    if (popup != null) {
		setReasonableLocation(popup, x, y);
		return true;
	    }
	}
	return false;
    } 
    
    public void showKeyboard() {
	if (requestFocus()) {
	    InputMethodManager imm = (InputMethodManager)
		activity
                .getSystemService(Context
				  .INPUT_METHOD_SERVICE);
	    imm.showSoftInput(this,
			      InputMethodManager
			      .SHOW_IMPLICIT);
	}
    }
    
    @Override
    protected void onDraw(Canvas canvas) {
	canvas.drawRGB(255, 255, 255);
	GRASP._log.draw(canvas, 0, 0);
	
	view.render(canvas);

	for (Shape segment : segments) {
	    segment.draw(canvas);
	}
	
	if (shape != null) {
	    shape.draw(canvas);
	}

	if (popup != null) {
	    GRASP.paint.setAlpha(64);
	    canvas.drawRect(0, 0, width, height,
			    GRASP.paint);
	    GRASP.paint.setAlpha(255);
	    popup.draw(canvas);
	}
    }
}
