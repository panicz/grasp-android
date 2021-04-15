import java.io.*;
import java.util.List;
import java.util.ArrayList;
import java.lang.StringBuilder;

class PeekingReader extends PushbackReader {
    public int peek() throws java.io.IOException {
	int c = read();
	unread(c);
	return c;
    }

    public PeekingReader(Reader in) {
	super(in);
    }

    public PeekingReader(Reader in, int size) {
	super(in, size);
    }
}

abstract class SExp {
    public String preceding_whitespace;
    //public abstract int width();
    //public abstract int height();
    protected abstract StringBuilder buildString(StringBuilder sb);
}

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
}

class SAtom extends SExp {
    public String name;

    public SAtom(String initial_whitespace,
		 String value) {
	preceding_whitespace = initial_whitespace;
	name = value;
    }
    
    @Override
    protected StringBuilder buildString(StringBuilder result) {
	result.append(name);
	return result;
    }

    @Override
    public String toString() {
	return name;
    }
}

class SDotted extends SList {
    public SExp tail;
    public String whitespace_before_dot;

    public SDotted(String initial_whitespace,
		   List<SExp> expressions,
		   String before_dot,
		   SExp dotted,
		   String final_whitespace) {
	super(initial_whitespace, expressions, final_whitespace);
	whitespace_before_dot = before_dot;
	tail = dotted;
    }
    
    @Override
    protected StringBuilder buildString(StringBuilder result) {
	result = super.buildString(result);
	result.deleteCharAt(result.length() - 1);
	result.append(' ');
	result.append('.');
	result.append(' ');
	result = tail.buildString(result);
	result.append(')');
	return result;
    }
}


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

		return new SDotted(initial_whitespace,
				   expressions,
				   whitespace/*before dot*/,
				   tail,
				   skip_whitespace());
	    }
	    else {
		input.unread(c);
		expressions.add(read_expression(whitespace));
	    }
	}
    }
}

class SExpParserExample {
    public static void main(String [] arguments) {
	try {
	    Reader input = new InputStreamReader(System.in);
	    
	    SExpReader s = new
		SExpReader(new PeekingReader(input, 4));
			   
	    SExp x = s.read_expression();
	    System.out.println(x);
	}
	catch(IOException e) {
	    System.out.println(e);
	}
    }
}
