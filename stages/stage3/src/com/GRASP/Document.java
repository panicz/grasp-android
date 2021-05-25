package com.GRASP;
import java.util.List;
import java.util.ArrayList;
import java.io.*;
import android.graphics.Canvas;
import java.lang.StringBuilder;


class Document extends Bit {
    public String path = null;
    public static List<Document> openedDocuments =
	new ArrayList<Document>();

    private Box root;
    
    public Document(String text) {
	openedDocuments.add(this);
	try {
	    Reader input = new
		StringReader(text);
	    SExpReader sexp =
		new SExpReader(new PeekingReader(input, 4));
	    SExp sexpr = sexp.read_expressions();
	    Bit content = sexpr.toBit();
	    assert(content instanceof Box);
	    root = (Box) content;
	} catch (IOException e) {
	    GRASP.log(e.toString());
	}
    }

    public Document(Box box) {
	root = box;
    }

    @Override
    public void render(Canvas canvas) {
	root.renderContents(canvas);
    }

    @Override
    public float width() {
	return root.width();
    }

    @Override
    public float height() {
	return root.height();
    }

    @Override
    protected StringBuilder buildString(StringBuilder sb) {
	return root.buildString(sb);
    }

    @Override
    public boolean insertAt(float x, float y, DragAround item) {
	return root.insertAt(x, y, item);
    }

    //public abstract Bit takeFrom(float x, float y);

    @Override
    public DragAround dragAround(float x, float y, TakeBit take) {
	return root.dragAround(x, y, take);
    }

    @Override
    public Bit shallow_copy() {
	return new Document(root);
    }

    @Override
    public Bit deep_copy() {
	return new Document((Box) root.deep_copy());
    }

    
}
