
package org.scheme.GRASP;

import gnu.lists.PairWithPosition;
import gnu.lists.Pair;
import gnu.lists.LList;

import android.graphics.Canvas;
//import android.graphics.Path;
import java.lang.StringBuilder;
import java.lang.Math;

class Box implements Bit {

    public Interline first_interline = null;
    
    final static float parenWidth = 20;
    final static float parenBar = 20;

    public static final float min_height =
	Atom.text_size + 2*Atom.vertical_margin;

    protected Space _following_space = null;

    public Box() {
	first_interline =
	    new Interline(0,
			  new Line(new Space()));
    }

    public Box(float w, float h) {
	this();
	trySetSize(w, h);
    }
    
    static final Shift shift = new Shift();
    
    @Override
    public Space following_space() {
	return _following_space;
    }

    @Override
    public void set_following_space(Space s) {
	_following_space = s;
    }
    
    @Override
    public int buildString(StringBuilder result) {
	return buildString(result, 0);
    }
    
    static void repeat(char c, int times,
		       StringBuilder result) {
	for (int i = 0; i < times; ++i) {
	    result.append(c);
	}
    }
    
    static boolean is_separator(char c) {
	return c == '(' || c == ')' || c == ' ';
    }

    static char last_char(StringBuilder sb) {
	return sb.charAt(sb.length()-1);
    }

    static void delete_last_char(StringBuilder sb) {
	sb.deleteCharAt(sb.length() - 1);
    }				 
    
    public int buildString(StringBuilder result,
			   int indent) {
	int longest_line_length = indent;

	result.append('(');

	for (Interline interline = first_interline;
	     interline != null;
	     interline = interline.following_line
		 .next_interline) {

	    repeat('\n', (int) (interline.height
				/ Atom.text_size),
		   result);
	    
	    if (interline.following_line == null) {
		break;
	    }

	    if (interline != first_interline) {
		result.append('\n');
		repeat(' ', indent, result);
	    }
	    
	    Line line = interline.following_line;
	    int line_indent = indent;
	    
	    for (Space preceding_space = line.first_space;
		 preceding_space != null;
		 preceding_space = preceding_space
		     .following_bit
		     .following_space()) {
		int spaces =
		    (int) Math
		    .floor(preceding_space.width
			   / (4*Space.min_width));
		if (spaces <= 0
		    && !is_separator(last_char(result))) {
		    spaces = 1;
		}
		repeat(' ', spaces, result);
		line_indent += spaces;
		if (preceding_space
		    .following_bit == null) {
		    break;
		}
		line_indent =
		    preceding_space.following_bit
		    .buildString(result, line_indent);	
	    }
	    if (line_indent > longest_line_length) {
		longest_line_length = line_indent;
	    }
	}

	if (last_char(result) == '\n') {
	    repeat(' ', longest_line_length - 1, result);
	}
	
	result.append(')');
	
	return longest_line_length;
    }

    @Override
    public String toString() {
	StringBuilder sb = new StringBuilder();
	buildString(sb);
	return sb.toString();
    }
    
    public void renderContents(Canvas canvas, int level,
			       Editor editor) {
	float accumulated_height = 0;
	
	for (Interline interline = first_interline;
	     interline != null;
	     interline = interline.following_line
		 .next_interline) {
	    /*
	    canvas.drawRect(20,accumulated_height,
			    40,accumulated_height
			    +interline.height,
			    GRASP.paint);
	    */
	    accumulated_height += interline.height;

	    if(interline.following_line == null) {
		break;
	    }
	    
	    Line line = interline.following_line;

	    float line_height = line.height();
	    
	    float accumulated_width = parenWidth;
	    
	    for (Space preceding_space = line.first_space;
		 preceding_space != null;
		 preceding_space =
		     preceding_space
		     .following_bit
		     .following_space()) {
		/*
		canvas.drawRect(accumulated_width,
				accumulated_height,
				accumulated_width
				+preceding_space.width,
				accumulated_height+20,
				GRASP.paint);
		*/
		accumulated_width +=
		    preceding_space.width;
		
		Bit bit = preceding_space.following_bit;
	       
		if (bit == null) {
		    break;
		}
		
		float w = bit.width();
		float h = bit.height();

		if (true
		    || !canvas.quickReject(accumulated_width,
					   accumulated_height,
					   accumulated_width+w,
					   accumulated_height+h,
					   Canvas.EdgeType.BW)) {
		
		    //canvas.save();
		    canvas.translate(accumulated_width,
				     accumulated_height);

		    //canvas.clipRect(0,0,w,h+100);
		    bit.render(canvas, level+1, editor);

		    canvas.translate(-accumulated_width,
				     -accumulated_height);

		    //canvas.restore();
		
		    accumulated_width += w;
		}
	    }
	    
	    accumulated_height += line_height;
	}

    }

    @Override
    public void render(Canvas canvas) {
	render(canvas, 0, null);
    }
    
    @Override
    public void render(Canvas canvas, int level,
		       Editor editor) {

	float w = first_interline.maximum_width();
	float h = height();
	
	int previous_color = GRASP.paint.getColor();
	GRASP.paint.setColor(previous_color + 0x111111);
	
	renderContents(canvas, level, editor);
	
	Paren.Left.render(canvas, GRASP.paint, h);
	canvas.translate(w+parenWidth, 0);

	Paren.Right.render(canvas, GRASP.paint, h);

	canvas.translate(-(w+parenWidth), 0);
	GRASP.paint.setColor(previous_color);
    }
    
    public void normalize() {
	for (Interline interline = first_interline;
	     interline != null;
	     interline = interline.following_line
		 .next_interline) {

	    interline.height = 0;
	    
	    Line line = interline.remove_empty_lines();
	    
	    if(line == null) {
		break;
	    }
	    
	    for (Space preceding_space = line.first_space;
		 preceding_space != null;
		 preceding_space =
		     preceding_space
		     .following_bit
		     .following_space()) {

		preceding_space.width = 16;
		
		Bit bit = preceding_space.following_bit;
	       
		if (bit == null) {
		    break;
		}

		if (bit instanceof Box) {
		    ((Box) bit).normalize();
		}
	    }	    
	}
    }

    
    @Override
    public float width() {
	return 2*parenWidth
	    + (first_interline == null
	       ? 0
	       : first_interline.maximum_width());
    }

    @Override
    public float min_width() {
	return 2*parenWidth
	    + (first_interline == null
	       ? 0
	       : first_interline.minimum_width());
    }

    @Override
    public float height() {
	return
	    Math.max(min_height,
		     first_interline == null
		     ? 0
		     : first_interline.onward_height());
    }

    @Override
    public float min_height() {
	return (first_interline == null)
	    ? 0
	    : first_interline.minimum_height();
    }

    @Override
    public float overwidth() {
	return width() - min_width();
    }

    @Override
    public float overheight() {
	return height() - min_height();	
    }

    @Override
    public void trySetSize(float w, float h) {
	Interline interline;
	float h_total = 0;
	float minh_total = 0;
	Line line = null;
	
	for (interline = first_interline;
	     interline != null;
	     interline = line.next_interline) {
	    
	    h_total += interline.height;

	    if (interline == first_interline) {
		line = interline.following_line;
	    } else {
		line = interline.remove_empty_lines();
	    }
	    
	    if(line == null) {
		break;
	    }
	    
	    Space space;
	    float w_total = 40;
	    float minw_total = 0;
	    Bit bit = null;
	    float max_minh = 0;
	    float max_h = min_height;
	    for (space = line.first_space;
		 space != null;
		 space = bit.following_space()) {

		w_total += space.width;
		minw_total += Math.min(Space.min_width,
				       space.width);
		
		bit = space.following_bit;
		
		if (bit == null) {
		    break;
		}

		w_total += bit.width();
		minw_total += bit.min_width();
		max_h = Math.max(max_h, bit.height());

		max_minh = Math.max(max_minh,
				    bit.min_height());
	    }
	    assert(space != null);
	    if (bit != null) {
		assert(bit.following_space() == null);
		space = new Space(0, null);
		space.width = 0;
		bit.set_following_space(space);
	    }
	    w_total -= space.width;
	    
	    if (w_total < w) {
		space.width = w - w_total;
	    }
	    else {
		// zmniejszanie szerokosci
	    }
	    h_total += max_h;
	    minh_total += max_minh;
	}

	
	if (line != null) {
	    assert(line.next_interline == null);
	    interline = line.next_interline =
		new Interline(0);
	}

	interline.height =
	    Math.max(0,
		     interline.height + (h - h_total));
    }
    
    // used in the public dragAround below,
    // overrode by Document
    protected Drag dragAround() {
	return new DragAround(this, 0, 0);
    }

    // used in the public dragAround below,
    // overrode by Document
    protected Drag resize(float x, float y) {
	return new Resize(this, x, y);
    }
    
    @Override
    public Drag dragAround(float x, float y,
			   TakeBit take) {
	float accumulated_height = 0;
	float maximum_width = 0;

	if (x < parenWidth) {
	    return dragAround();
	}
	
	for (Interline interline = first_interline;
	     interline != null;
	     interline = interline.following_line
		 .next_interline) {

	    accumulated_height += interline.height;

	    if(interline.following_line == null) {
		break;
	    }
	    
	    Line line = interline.following_line;

	    float line_height = line.height();
	    
	    float accumulated_width = parenWidth;
	    
	    for (Space preceding_space = line.first_space;
		 preceding_space != null;
		 preceding_space =
		     preceding_space
		     .following_bit
		     .following_space()) {

		accumulated_width
		    += preceding_space.width;

		Bit bit = preceding_space.following_bit;

		if (bit == null) {
		    break;
		}
		
		float w = bit.width();
		float h = bit.height();

		assert(h <= line_height);
		
		float rx = x - accumulated_width;
		float ry = y - accumulated_height;
		
		if (0 <= rx && rx <= w
		    && 0 <= ry && ry <= h) {
		    Drag nested =
			bit.dragAround(rx, ry, take);
		    if (nested != null) {
			if (nested instanceof DragAround
			    && ((DragAround)nested).target
			    == bit) {
			    ((DragAround)nested).target =
				take
				.from(preceding_space,
				      line);
			}
			else if(nested instanceof Resize
				&& ((Resize)nested).parent
				== null) {
			    Resize r = (Resize)nested;
			    r.parent = this;
			    r.xp = accumulated_width;
			    r.yp = accumulated_height;
			    r.preceding_space =
				preceding_space;
			    r.line = line;
			}
			shift.set(accumulated_width,
				  accumulated_height);
			return nested.outwards(shift);
		    }
		    return null;
		}
		accumulated_width += w;
	    }
	    
	    if (accumulated_width > maximum_width) {
		maximum_width = accumulated_width;
	    }
	    accumulated_height += line_height;
	}
	if (maximum_width <= x
	    && x < maximum_width + parenWidth) {
	    return resize(x, y);
	    //return dragAround();
	}
	return null;
    }

    // used from insertAt method, overrode by Document
    protected Space insertLast(Interline last_interline,
			       Line line,
			       float x, float y,
			       float accumulated_height,
			       DragAround target,
			       Ref<Line> ln) {
	return null;
    }


    @Override
    public Bit itemAt(float x, float y) {
	
	float accumulated_height = 0;
	Interline interline;
	Line line = null;
	
	for (interline = first_interline;
	     interline != null;
	     interline = interline.following_line
		 .next_interline) {
	    
	    accumulated_height += interline.height;

	    if (y < accumulated_height) {
		return this;
	    }

	    line = interline.following_line;

	    if(line == null) {
		break;
	    }
	    
	    float line_height = line.height();

	    if (y >= accumulated_height + line_height) {
		accumulated_height += line_height;
		continue;
	    }
	    
	    float accumulated_width = parenWidth;
	    
	    for (Space last_space = line.first_space;
		 last_space != null;
		 last_space = last_space.following_bit
		     .following_space()) {

		if (x <= (accumulated_width
			  + last_space.width)
		    || last_space.following_bit == null
		    ) {
		    return this;
		}

		accumulated_width += last_space.width;
	       
		Bit bit = last_space.following_bit;
	       
		if (bit == null) {
		    break;
		}
		
		float w = bit.width();
		float h = bit.height();

		float rx = x - accumulated_width;
		float ry = y - accumulated_height;
		
		if (0 <= rx && rx <= w
		    && 0 <= ry && ry <= h) {
		    return bit.itemAt(rx, ry);
		}

		accumulated_width += w;
		rx -= w;
		
		if (bit.following_space() == null) {
		    return this;
		}
	    }
	    
	    accumulated_height += line_height;
	}

	return this;
    }

    public AtomEditor editItemAt(float x, float y) {
	
	float accumulated_height = 0;
	Interline interline;
	Line line = null;
	
	for (interline = first_interline;
	     interline != null;
	     interline = interline.following_line
		 .next_interline) {
	    
	    accumulated_height += interline.height;

	    if (y < accumulated_height) {
		return null;
	    }

	    line = interline.following_line;

	    if(line == null) {
		break;
	    }
	    
	    float line_height = line.height();

	    if (y >= accumulated_height + line_height) {
		accumulated_height += line_height;
		continue;
	    }
	    
	    float accumulated_width = parenWidth;
	    
	    for (Space last_space = line.first_space;
		 last_space != null;
		 last_space = last_space.following_bit
		     .following_space()) {

		if (x <= (accumulated_width
			  + last_space.width)
		    || last_space.following_bit == null
		    ) {
		    return null;
		}

		accumulated_width += last_space.width;
	       
		Bit bit = last_space.following_bit;
	       
		if (bit == null) {
		    break;
		}
		
		float w = bit.width();
		float h = bit.height();

		float rx = x - accumulated_width;
		float ry = y - accumulated_height;
		
		if (0 <= rx && rx <= w
		    && 0 <= ry && ry <= h) {
		    if (bit instanceof Box) {
			Box box = (Box) bit;
			return box.editItemAt(rx, ry);
		    }
		    else if (bit instanceof Atom) {
			return new
			    AtomEditor(last_space,
				       line);
		    }
		    else {
			GRASP.log("unsupported bit "+
				  this);
			return null;
		    }
		}

		accumulated_width += w;
		rx -= w;
		
		if (bit.following_space() == null) {
		    return null;
		}
	    }
	    
	    accumulated_height += line_height;
	}

	return null;
    }

    
    @Override
    public Space insertAt(float x, float y,
			  DragAround target,
			  Ref<Line> ln) {
	
	float accumulated_height = 0;
	Interline interline;
	Line line = null;
	
	for (interline = first_interline;
	     interline != null;
	     interline = interline.following_line
		 .next_interline) {
	    
	    accumulated_height += interline.height;

	    if (y < accumulated_height) {
		Line added = interline
		    .insert_line_with(target, x, y);
		if (added == null) {
		    return null;
		}

		
		return added.first_space;
	    }

	    line = interline.following_line;

	    if(line == null) {
		break;
	    }
	    
	    float line_height = line.height();

	    if (y >= accumulated_height + line_height) {
		accumulated_height += line_height;
		continue;
	    }
	    
	    float accumulated_width = parenWidth;
	    
	    for (Space last_space = line.first_space;
		 last_space != null;
		 last_space = last_space.following_bit
		     .following_space()) {

		if (x <= (accumulated_width
			  + last_space.width)
		    || last_space.following_bit == null
		    ) {
		    shift.set(accumulated_width,
			      accumulated_height);
		    boolean result = last_space
			.insertAt(x - accumulated_width,
				  y - accumulated_height,
				  (DragAround) target
				  .inwards(shift));
		    if(ln != null) {
			ln.ref = line;
		    }
		    
		    float new_height = line.height();
		    float increase =
			new_height - line_height;
		    assert(increase >= 0);

		    if (line.next_interline != null) {
			line.next_interline
			    .remove_empty_lines();

			line.next_interline.height
			    -= Math.min(increase,
					line
					.next_interline
					.height);
		    }
		    
		    return result ? last_space : null;
		}

		accumulated_width += last_space.width;
	       
		Bit bit = last_space.following_bit;
	       
		if (bit == null) {
		    break;
		}
		
		float w = bit.width();
		float h = bit.height();

		float rx = x - accumulated_width;
		float ry = y - accumulated_height;
		
		if (0 <= rx && rx <= w
		    && 0 <= ry && ry <= h) {
		    shift.set(accumulated_width,
			      accumulated_height);
		    return bit
			.insertAt(rx, ry,
				  (DragAround) target
				  .inwards(shift),
				  ln);
		}

		accumulated_width += w;
		rx -= w;
		
		if (bit.following_space() == null) {
		    bit.set_following_space(new
					    Space(rx));
		    shift.set(accumulated_width,
			      accumulated_height);
		    boolean result = bit.following_space()
			.insertAt(rx, ry,
				  (DragAround) target
				  .inwards(shift));
		    if(ln != null) {
			ln.ref = line;
		    }

		    float new_height = line.height();
		    float increase =
			new_height - line_height;
		    assert(increase >= 0);
	
		    if (line.next_interline != null) {
			line.next_interline
			    .remove_empty_lines();

			line.next_interline.height
			    -= Math.min(increase,
					line
					.next_interline
					.height);
		    }
		    
		    return result
			? bit.following_space()
			: null;
		}
	    }
	    
	    accumulated_height += line_height;
	}

	return insertLast(interline, line, x, y,
			  accumulated_height,
			  target, ln);
    }

    public Bit shallow_copy() {
	Box copy = new Box();
	copy.first_interline = first_interline;
	return copy;
    }

    public Bit deep_copy() {
	Box copy = new Box();
	if(first_interline != null) {
	    copy.first_interline =
		first_interline.deep_copy();
	}
	
	if (_following_space != null) {
	    copy.set_following_space(_following_space
				     .deep_copy());
	}

	return copy;	
    }

    static DragAround throwAround =
	new DragAround(null, 0, 0);
    
    void createBoxWithElementsFrom(float left,
				   float top,
				   float right,
				   float bottom) {
	assert(bottom > top);
	assert(right > left);

	float bw = right-left;
	float bh = bottom-top;
	Box box = new Box(bw, bh);
	
	float accumulated_height = 0;

	for (Interline interline = first_interline;
	     interline != null;
	     interline = interline.following_line
		 .next_interline) {

	    accumulated_height += interline.height;
	    
	    if (accumulated_height > bottom) {
		break;
	    }
	    
	    if(interline.following_line == null) {
		break;
	    }
	    
	    Line line = interline.following_line;

	    float line_height = line.height();
	    
	    float accumulated_width = parenWidth;

	    int items_to_move = 0;
	    Space initial_space = null;
	    float position = 0;
	    
	    for (Space preceding_space = line.first_space;
		 preceding_space != null;
		 preceding_space =
			preceding_space
			.following_bit
			.following_space()) {
		accumulated_width
		    += preceding_space.width;

		if (accumulated_width > right) {
		    break;
		}
		
		Bit bit = preceding_space.following_bit;
		
		if (bit == null) {
		    break;
		}
		
		float w = bit.width();
		float h = bit.height();

		if(left <= accumulated_width
		   && accumulated_width + w <= right
		   && top <= accumulated_height
		   && accumulated_height + h <= bottom) {
		    if (initial_space == null) {
			initial_space = preceding_space;
			position = accumulated_width
			    - left
			    - preceding_space.width;
		    }
		    ++items_to_move;
		}

		accumulated_width += w;	
	    }

	    //GRASP.log("items to move: "+items_to_move);
	    
	    for (int i = 0; i < items_to_move; ++i) {
		// zabieramy sobie bit stad
		// i go wkladamy do nowego
		// puelka
		throwAround.x =
		    position + initial_space.width;
		
		throwAround.target =
		    initial_space
		    .remove_following_bit(line);
		    
		throwAround.y =
		    accumulated_height;
		box.insertAt(throwAround.x,
			     accumulated_height-top,
			     throwAround,
			     null);
	    }
	    
	    accumulated_height += line_height;
	}
	// a na koniec umieszczamy nowe pudelko
	// w odpowiednim miejscu
	throwAround.target = box;
	throwAround.x = left;
	throwAround.y = top;
	insertAt(left, top, throwAround, null);

    }

    
    void createBox(float left, float top,
		   float right, float bottom) {
	// 1. znajdz najglebsze pudelko, do ktorego
	// naleza (left, top) i (right, bottom)
	// 2. stworz nowe pudelko, i umiesc w nim
	// wszystkie elementy z najglebszego pudelka,
	// ktore sa calkowicie zawarte
	assert(bottom > top);
	assert(right > left);

	float accumulated_height = 0;
 
	for (Interline interline = first_interline;
	     interline != null;
	     interline = interline.following_line
		 .next_interline) {

	    accumulated_height += interline.height;
	    
	    if (accumulated_height > bottom) {
		break;
	    }
	    
	    if(interline.following_line == null) {
		break;
	    }
	    
	    Line line = interline.following_line;

	    float line_height = line.height();
	    
	    float accumulated_width = parenWidth;
	    
	    for (Space preceding_space = line.first_space;
		 preceding_space != null;
		 preceding_space =
		     preceding_space
		     .following_bit
		     .following_space()) {
		accumulated_width
		    += preceding_space.width;
		
		Bit bit = preceding_space.following_bit;

		if (accumulated_width > right) {
		    break;
		}
		
		if (bit == null) {
		    break;
		}
		
		float w = bit.width();
		float h = bit.height();

		if (bit instanceof Box
		    && accumulated_width <= left
		    && right <= accumulated_width + w
		    && accumulated_height <= top
		    && bottom <= accumulated_height + h) {
		    Box x = (Box) bit;
		    x.createBox(left-accumulated_width,
				top-accumulated_height,
				right-accumulated_width,
				bottom
				-accumulated_height);
		    return;
		}
		accumulated_width += w;	
	    }   
	    accumulated_height += line_height;
	}

	createBoxWithElementsFrom(left, top,
				  right, bottom);
    }

    @Override
    public Object toKawa() {
	Object head = LList.Empty;
	Pair last_tail = null;

	for (Interline interline = first_interline;
	     interline != null;
	     interline = interline.following_line
		 .next_interline) {

	    if (interline.following_line == null) {
		break;
	    }
	    
	    Line line = interline.following_line;
	    
	    for (Space preceding_space = line.first_space;
		 preceding_space != null;
		 preceding_space =
		     preceding_space
		     .following_bit
		     .following_space()) {
		
		Bit bit = preceding_space.following_bit;
		
		if (bit == null) {
		    break;
		}

		if (last_tail == null) {
		    assert(head == LList.Empty);
		    last_tail = new PairWithPosition(bit.toKawa(),
						     LList.Empty);
		    head = last_tail;
		}
		else {
		    PairWithPosition newtail =
			new PairWithPosition(bit.toKawa(), LList.Empty);
		    last_tail.setCdr(newtail);
		    last_tail = newtail;
		}
	    }
	}
	return head;
    }

    public Indexable get(int index, Ref<Line> ln) {
	Line line = null;
	int current = 0;
	
	for (Interline interline = first_interline;
	     interline != null;
	     interline = line.next_interline) {

	    line = interline.following_line;

	    if (line == null) {
		GRASP.log("line for "+index+" in "+this+" is null");
		return null;
	    }
		
	    Bit bit = null;
		
	    for (Space space = line.first_space;
		 space != null;
		 space = bit.following_space()) {

		bit = space.following_bit;
		if (bit == null) {
		    break;
		}
		
		if (index == current) {
		    if(ln != null) {
			ln.ref = line;
		    }
		    return space;
		}
		
		++current;
	       

		if (index == current) {
		    if(ln != null) {
			ln.ref = line;
		    }
		    return bit;
		}
		++current;
	    }
	}
	GRASP.log("index "+index+" in "+this+" not found");

	return null;
    }
}
