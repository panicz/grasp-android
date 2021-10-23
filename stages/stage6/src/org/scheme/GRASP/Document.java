package org.scheme.GRASP;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.io.*;
import android.graphics.Canvas;
import java.lang.StringBuilder;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Calendar;


class Document extends Box implements DocumentOperations {
    public File file = null;
    public static List<Document> openedDocuments =
	new ArrayList<Document>();

    public static Document fromFile(File file) {
	Iterator<Document> it =
	    openedDocuments.iterator();
	
	while(it.hasNext()) {
	    Document doc = it.next();
	    if (doc.file.getPath()
		.equals(file.getPath())) {
		return doc;
	    }
	}
	try {
	    Reader input = new FileReader(file);
	    SExpReader sexp =
		new SExpReader(new
			       PeekingReader(input, 4));
	    	    
	    SExp sexpr = sexp.read_expressions();
	    Bit content = sexpr.toBit();
	    assert(content instanceof Box);
	    Document result = fromBox((Box) content);
	    result.file = file;
	    return result;
	} catch (IOException e) {
	    GRASP.log(e.toString());
	    return null;
	}
    }
    
    static final float min_space_between_bits = 100.0f;
    
    public Document() {
	openedDocuments.add(this);
    }

    public Document(File file) {
	this();
	this.file = file;

	first_interline = new Interline(0, null);
	_following_space = new Space(0, null);
    }
    
    public static Document fromBox(Box prototype) {
	Document document = new Document();
	document.first_interline =
	    prototype.first_interline;
	document._following_space =
	    prototype.following_space();
	document.preserve_distance_between_elements();
	return document;
    }

    static DateFormat date =
	new SimpleDateFormat("yyyy-MM-dd@HH:mm:ss");
    
    public static Document createNew() {
	Date now = Calendar.getInstance().getTime();
	
	String file_base = GRASP.instance.getFilesDir()
	    + File.separator + date.format(now);

	String path = file_base + ".grasp";
       
	File file = new File(path);

	for (int i = 0; file.exists(); ++i) {
	    path = file_base + "-" + i + ".grasp";
	    GRASP.log("retrying with "+path);
	    file = new File(path);
	}

	GRASP.log("created "+path);
	
	return new Document(file);
    }

    public static void close(Document x) {
	if (x.file == null) {
	    return;
	}
	openedDocuments.remove(x);
    }

    
    @Override
    public void render(Canvas canvas, int level,
		       Editor editor) {
	renderContents(canvas, level, editor);
    }

    @Override
    public Bit shallow_copy() {
	return fromBox(this);
    }

    @Override
    public Bit deep_copy() {
	return fromBox((Box) super.deep_copy());
    }

    // used in Box's public dragAround method
    @Override
    protected Drag dragAround() {
	// prevent the whole document from being dragged around
	return null;
    }

    @Override
    public void trySetSize(float x, float y) {}

    // used in Box's public dragAround method
    @Override
    protected Drag resize(float x, float y) {
	// prevent the whole document from being resized
	return null;
    }

    // used in Box's insertAt method
    @Override
    protected Space insertLast(Interline last_interline,
			       Line line,
			       float x, float y,
			       float accumulated_height,
			       DragAround target,
			       Ref<Line> ln) {
	if (line != null) {
	    if (line.next_interline == null) {
		line.next_interline = new
		    Interline(y - accumulated_height);
	    }
	    last_interline = line.next_interline;
	}
	// make sure that the dragged box is added
	// if it was dropped below the last expression
	// in the document
	Line added = last_interline
	    .insert_line_with(target, x, y);

	if (added == null) {
	    return null;
	}

	if(ln != null) {
	    ln.ref = added;
	}
	
	return added.first_space;
    }

    public void preserve_distance_between_elements() {
	for (Interline interline = first_interline;
	     interline != null;
	     interline = interline
		 .following_line.next_interline) {

	    if(interline.height < min_space_between_bits){
		interline.height = min_space_between_bits;
	    }
	    
	    do {
		Line line = interline.following_line;
		if (line == null) {
		    return;
		}
		
		if (line.first_space == null
		    || (line.first_space
			.following_bit == null)) {
		    interline.remove_following_line();
		    interline.height =
			min_space_between_bits;
		    continue;
		}
		Space preceding_space = null;
		for (preceding_space = line.first_space;
		     preceding_space != null
			 && (preceding_space
			     .following_bit != null);
		     preceding_space =
			 preceding_space
			 .following_bit
			 .following_space()) {
		    if (preceding_space.width
			< min_space_between_bits
			&& (preceding_space
			    != line.first_space)) {
			preceding_space.width =
			    min_space_between_bits;
		    }
		}
		if(preceding_space != null) {
		    preceding_space.width = 0;
		}
		break;
	    } while(true);
	}
    }
    
    @Override
    public Space insertAt(float x, float y,
			  DragAround target,
			  Ref<Line> ln) {
	Space result = super.insertAt(x, y, target, ln);
	if (result != null) {
	    preserve_distance_between_elements();
	}
	else {
	    // powinnismy cofnac historie sprzed dragniecia
	}
	return result;
    }


    // we return the DragAround here because of laziness
    // - it has all the fields we need, but we actually
    // don't use any of its methods
    public DragAround topLevelItemAt(float x, float y) {
	float accumulated_height = 0;
	Line line;
	for (Interline interline = first_interline;
	     interline != null;
	     interline = line.next_interline) {

	    accumulated_height += interline.height;

	    if (accumulated_height > y) {
		break;
	    }
	   
	    line = interline.following_line;
	    
	    if(line == null) {
		break;
	    }

	    float accumulated_width = 0;
	    float max_height = 0;
	    Bit bit;
	    for (Space preceding_space = line.first_space;
		 preceding_space != null;
		 preceding_space =
		     bit.following_space()) {

		accumulated_width
		    += preceding_space.width;

		if (accumulated_width > x) {
		    break;
		}
		
		bit = preceding_space.following_bit;
		
		if (bit == null) {
		    break;
		}

		float w = bit.width();
		float h = bit.height();

		if (accumulated_height <= y
		    && y <= accumulated_height + h
		    && accumulated_width <= x
		    && x <= accumulated_width + w) {
		    return new
			DragAround(bit,
				   accumulated_width,
				   accumulated_height);
		}

		accumulated_width += w;
		if (h > max_height) {
		    max_height = h;
		}
	    }
	    
	    accumulated_height += max_height;
	}
	
	return null;
    }

    @Override
    public int buildString(StringBuilder sb, int indent) {
	int result = super.buildString(sb, indent);
	int left = sb.indexOf("(");
	int right = sb.lastIndexOf(")");
	if (left >= 0 && right > left) {
	    sb.deleteCharAt(right);
	    sb.deleteCharAt(left);
	}
	while(last_char(sb) == ' ') {
	    delete_last_char(sb);
	}
	return result;
    }

    @Override
    public Track track(float x, float y) {
	Track track = new Track();

	Box that = this, last = null;

	float vbase = 0, hbase = 0;
	
	while(that != last) {
	    last = that;
	    int current = 0;
	    Line line = null;
	    float vfront = 0;
	    
	    lines:
	    for (Interline interline = that.first_interline;
		 interline != null;
		 interline = line.next_interline) {
		float hfront = 0;
		vfront += interline.height;
		
		line = interline.following_line;

		if (line == null) {
		    track.dx = x;
		    track.dy = y;
		    track.sx = hbase+hfront;
		    track.sy = vbase+vfront;
		    return track;
		}
		
		Bit bit = null;

		float lineheight = Box.min_height;

		hfront += parenWidth;

		int spaceindex = 0;
		
		for (Space space = line.first_space;
		     space != null;
		     space = bit.following_space()) {
		    		    
		    bit = space.following_bit;

		    if ((hfront <= x && x <= hfront + space.width)) {
			assert(current % 2 == 0);
			spaceindex = current;
		    }

		    if (bit == null) {
			break;
		    }

		    hfront += space.width;
		    
		    ++current;

		    float w = bit.width();
		    float h = bit.height();

		    if (h > lineheight) {
			lineheight = h;
		    }
		    
		    if (hfront <= x && x <= hfront + w
			&& vfront <= y && y <= vfront + h) {
			track.turns.add(current);
			if (bit instanceof Box) {
			    last = that; 
			    that = (Box) bit;
			    y -= vfront;
			    x -= hfront;
			    hbase += hfront;
			    vbase += vfront;
			    break lines;
			}
			else {
			    track.dx = x - hfront;
			    track.dy = y - vfront;
			    track.sx = hbase+hfront;
			    track.sy = vbase+vfront;
			    return track;
			}
		    }

		    hfront += w;

		    ++current;
		}

		if (vfront + lineheight > y) {
		    if (x <= hfront && spaceindex >= 0) {
			assert(spaceindex % 2 == 0);
			track.turns.add(spaceindex);
		    }
		    else {
			assert(current % 2 == 0);
			track.turns.add(current);
		    }
		    track.dx = x;
		    track.dy = y;
		    track.sx = hbase;
		    track.sy = vbase;

		    return track;
		}
		vfront += lineheight;
	    }
	}

	return track;
    }

    @Override
    public String toString() {
	    return "Document";
    }

    Ref<Line> line = new Ref<Line>(null);

    @Override
    public Indexable refer(Track track) {
	Indexable result = this;
	int size = track.turns.size();
	for(int i = 0; i < size; ++i) {
	    int turn = track.turns.get(i);
	    if (result instanceof Box) {
		result = ((Box)result).get(turn, line);
	    }
	    else {
		GRASP.log("index "+i+" of "+track+" is not a box");
		break;
	    }
	}
	return result;
    }
    
    @Override
    public Bit take(Track track) {
	int size = track.turns.size();
	if (size == 0) {
	    return null;
	}

	int last = track.turns.get(size-1);

	if(last % 2 == 0) {
	    // for tracks that lead to spaces,
	    // we take the box which contaibs that space
	    track.turns.remove(size-1);

	    Bit result = take(track);
	    track.turns.add(size-1, last);
	    return result;
	}
	
	track.turns.set(size-1, last-1);

	Indexable thing = refer(track);
	if(!(thing instanceof Space)) {
	    GRASP.log("no space at "+track+", last = "+last);
	    track.turns.set(size-1, last);
	    return null;
	}

	Bit result = ((Space)thing).remove_following_bit(line.ref);
	track.turns.set(size-1, last);
	
	return result;
    }

    @Override
    public Bit copy(Track track) {
	Indexable reference = refer(track);
	if (reference == this
	    || reference == null
	    || !(reference instanceof Bit)) {
	    return null;
	}
	Bit copy = ((Bit)reference).shallow_copy();
	copy.set_following_space(null);
	return copy.deep_copy();
    }

    @Override
    public void insert(Bit bit, Track track) {
	int size = track.turns.size();
	if (size == 0
	    || track.turns.get(size-1)%2 != 0) {
	    GRASP.log("unable to insert "+bit+" at "+track);
	    return;
	}
	
	Box target = this;

	for(int i = 0; i < size; ++i) {
	    int turn = track.turns.get(i);
	    Indexable tip = target.get(turn, line);
	    if (tip instanceof Space) {
		assert(i == size-1);
		((Space)tip).insertAfter(track.dx, bit);
	    }
	}
    }
}
