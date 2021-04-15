import java.io.*;

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
