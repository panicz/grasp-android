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

    public Document() {
	openedDocuments.add(this);
    }
    
    public static Document fromBox(Box prototype) {
	Document document = new Document();
	document.first_interline = prototype.first_interline;
	document._following_space = prototype.following_space();
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
	return fromBox((Box) deep_copy());
    }

    @Override
    protected DragAround dragAround() {
	return null;
    }

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
	return last_interline.insert_line_with(target);
    }

}
