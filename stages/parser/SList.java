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
}
