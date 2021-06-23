package com.GRASP;
import java.util.List;
import java.util.ArrayList;
import java.io.*;
import android.graphics.Canvas;
import java.lang.StringBuilder;


class Document extends Box {
    public String path = null;
    public static List<Document> openedDocuments =
	new ArrayList<Document>();

    static final float min_space_between_bits = 100.0f;
    
    public Document() {
	openedDocuments.add(this);
    }
    
    public static Document fromBox(Box prototype) {
	Document document = new Document();
	document.first_interline = prototype.first_interline;
	document._following_space = prototype.following_space();
	document.preserve_distance_between_elements();
	return document;
    }

    public static Document fromSource(String text) {
	try {
	    Reader input = new
		StringReader(text);
	    SExpReader sexp =
		new SExpReader(new PeekingReader(input, 4));
	    SExp sexpr = sexp.read_expressions();
	    Bit content = sexpr.toBit();
	    assert(content instanceof Box);
	    return fromBox((Box) content);
	} catch (IOException e) {
	    GRASP.log(e.toString());
	    return null;
	}
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


    // used in Box's insertAt method
    @Override
    protected boolean insertLast(Interline last_interline,
				 Line line,
				 float x, float y,
				 float accumulated_height,
				 DragAround target) {
	if (line != null) {
	    if (line.next_interline == null) {
		line.next_interline = new
		    Interline(y - accumulated_height, null);
	    }
	    last_interline = line.next_interline;
	}
	// make sure that the dragged box is added
	// if it was dropped below the last expression
	// in the document
	return last_interline.insert_line_with(target);
    }

    private void preserve_distance_between_elements() {
	for (Interline interline = first_interline;
	     interline != null;
	     interline = interline.following_line.next_interline) {

	    if(interline.height < min_space_between_bits) {
		interline.height = min_space_between_bits;
	    }
	    
	    if(interline.following_line == null) {
		break;
	    }
	    
	    Line line = interline.following_line;

	    for (Space preceding_space = line.first_space;
		 preceding_space != null
		     && preceding_space.following_bit != null;
		 preceding_space =
		     preceding_space
		     .following_bit
		     .following_space()) {
		if (preceding_space.width < min_space_between_bits
		    && preceding_space != line.first_space) {
		    preceding_space.width = min_space_between_bits;
		}
	    }
	}
    }
    
    @Override
    public boolean insertAt(float x, float y, DragAround target) {
	boolean result = super.insertAt(x, y, target);
	if (result) {
	    preserve_distance_between_elements();
	}
	else {
	    // powinnismy cofnac historie sprzed dragniecia
	}
	return result;
    }
    
}
