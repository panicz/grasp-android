package com.GRASP;

import java.io.*;
import java.util.List;
import java.util.ArrayList;
import java.lang.StringBuilder;


class SExpReader {
    PeekingReader input;

    public SExpReader(PeekingReader input) {
	this.input = input;
    }

    public static boolean is_delimiter(int c) {
	return Character.isWhitespace(c)
	    || c == -1
	    || c == '('
	    || c == ')'
	    || c == '"'
	    || c == ';';
    }
    
    String skip_whitespace() throws java.io.IOException {
	StringBuilder result = new StringBuilder();
	for (int c = input.read(); c != -1; c = input.read()) {
	    if (Character.isWhitespace(c)) {
		result.append((char) c);
	    }
	    else if(c == ';') {
		do {
		    result.append((char) c);
		    c = input.read();
		} while(c != '\n' && c != -1);
		if (c == '\n') {
		    result.append((char) c);
		    continue;
		}
	    }
	    else if(c == '#'
		    && input.peek() == '|') {
		result.append((char) c);
		c = input.read();
		do {
		    result.append((char) c);
		    c = input.read();
		} while(c != -1
			&& c != '|'
			&& input.peek() != '#');
		if (c == '|') {
		    result.append(c);
		    c = input.read();
		    if (c == '#') {
			result.append((char) c);
			continue;
		    }
		}
	    }
	    else if(c == '#'
		    && input.peek() == ';') {
	    }
	    else {
		input.unread(c);
		break;
	    }
	}
	return result.toString();
    }

    public SAtom read_atom(String initial_whitespace)
	throws java.io.IOException {
	StringBuilder result = new StringBuilder();
	int c = input.read();

	while (!is_delimiter(c)) {
	    result.append((char) c);
	    c = input.read();
	}

	input.unread(c);
	return new SAtom(initial_whitespace,
			 result.toString());
	
    }
    
    public SExp read_expression(String initial_whitespace)
	throws java.io.IOException {
	if(initial_whitespace == null) {
	    initial_whitespace = skip_whitespace();
	}
	int c = input.read();
	if (c == '(') {
	    return read_expressions(initial_whitespace);
	}
	
	/*if (c == '"') {
	    return read_string(initial_whitespace);
	    }*/
	input.unread(c);
	return read_atom(initial_whitespace);
    }

    public SExp read_expression()
	throws java.io.IOException {
	return read_expression(null);
    }
    
    public SExp read_expressions(String initial_whitespace)
	throws java.io.IOException {
	List<SExp> expressions = new ArrayList<SExp>();
	
	while(true) {
	    String whitespace = skip_whitespace();
	    
	    int c = input.read();
	    if (c == ')' || c == -1) {
		return new SList(initial_whitespace,
				 expressions,
				 /*ultimate*/whitespace);
	    }
	    else if(c == '.' && is_delimiter(input.peek())) {
		SExp tail = read_expression();
		String ultimate_whitespace = skip_whitespace();
		c = input.read();
		assert(c == ')' || c == -1);
		return new SDotted(initial_whitespace,
				   expressions,
				   whitespace/*before dot*/,
				   tail,
				   ultimate_whitespace);
	    }
	    else {
		input.unread(c);
		expressions.add(read_expression(whitespace));
	    }
	}
    }    
}
