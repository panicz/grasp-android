import java.util.List;
import java.lang.StringBuilder;

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

    @Override
    public Bit toBit() {
	Box box = (Box) super.toBit();
	// ...
	return box;
    }

}
