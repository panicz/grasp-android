package com.GRASP;
import java.util.List;
import java.util.ArrayList;
import java.io.*;

class Document implements Operations {
    public String path = null;
    public static List<Document> openedDocuments =
	new ArrayList<Document>();

    public Box root;
    
    public Document() {
	openedDocuments.add(this);
	try {
	    Reader input = new
		StringReader("\n"
			     +"(define (! n)\n"
			     +" (if (= n 0)\n"
			     +"   1\n"
			     +"  (* n (! (- n 1)))))\n\n\n"
			     +"(e.g. (! 5) \u21d2 120)\n\n\n"
			     +"(e.g.\n"
			     +"  (= (! 5)\n"
			     +"  (* 1 2 3 4 5)))\n"
			     );
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
    
}
