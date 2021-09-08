package com.GRASP;
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


class Document extends Box {
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
    public void render(Canvas canvas) {
	renderContents(canvas);
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
    public  void trySetSize(float x, float y) {}

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

}
