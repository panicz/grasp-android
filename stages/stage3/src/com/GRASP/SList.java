package com.GRASP;

import java.util.List;
import java.lang.StringBuilder;

class SList extends SExp {
    public List<SExp> elements;
    public String ultimate_whitespace;

    public SList(String initial_whitespace,
		 List<SExp> expressions,
		 String final_whitespace) {
	preceding_whitespace = initial_whitespace;
	elements = expressions;
	ultimate_whitespace = final_whitespace;
    }
    
    @Override
    protected StringBuilder buildString(StringBuilder result) {
	result.append('(');
	for (SExp s : elements) {
	    s.buildString(result);
	    result.append(' ');
	}
	result.deleteCharAt(result.length() - 1);
	result.append(')');
	return result;
    }
    
    @Override
    public String toString() {
	return buildString(new StringBuilder()).toString();
    }

    @Override
    public Bit toBit() {
	Box box = new Box();

	Bit previous_bit = null;
	Line current_line = null;
	
	for (SExp sexp : elements) {
	    Bit current_bit = sexp.toBit();
	    String pws = sexp.preceding_whitespace;
	    int interline_rows = S.lines(pws);
	    int space_columns = S.ultimate_line_width(pws);
	    Space space = new Space(space_columns, current_bit);
	    if (previous_bit == null) {
		box.first_interline = new
		    Interline(interline_rows, new Line(space));
		current_line = box.first_interline.following_line;
	    }
	    else if(pws.contains("\n")) {
		current_line =
		    current_line.appendLine(interline_rows,
					    space);
	    }
	    else {
		previous_bit.following_space = space;
	    }
	    previous_bit = current_bit;
	}

	Interline last_interline = new
		Interline(S.lines(ultimate_whitespace)); 
	
	if(current_line == null) {
	    box.first_interline = last_interline;
	}
	else {
	    current_line.next_interline = last_interline;
	}
	
	return box;
    }
}
