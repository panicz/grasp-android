
class Atom extends Bit {
    /*@NonNull*/ public String text;
    
    @Override public float width() {
	return 0;//GRASP.paint.measureText(text);
    }
    
    @Override public float height() {
	return 0;//GRASP.paint.getTextSize();
    }
    
    public Atom(String value) {
	text = value;
    }
}
