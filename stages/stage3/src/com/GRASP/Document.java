package com.GRASP;
import java.util.List;
import java.util.ArrayList;
import java.io.*;

class Document implements Operations {
    public String path = null;
    public static List<Document> openedDocuments =
	new ArrayList<Document>();

    public Bit root;
    
    public Document() {
	openedDocuments.add(this);
	try {
	    Reader input = new
		StringReader("\n"
			     +"(define (! n)\n"
			     +" (if (= n 0)\n"
			     +"   1\n"
			     +"  (* n (! (- n 1)))))\n"
			     );
	    SExpReader sexp =
		new SExpReader(new PeekingReader(input, 4));
	    SExp sexpr = sexp.read_expression();
	    root = sexpr.toBit();
	} catch (IOException e) {
	    GRASP.log(e.toString());
	}
    }
    
}
