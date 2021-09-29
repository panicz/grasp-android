package org.scheme.GRASP;

import java.util.List;


class ShowOpenedDocuments implements Action {
    class SwitchToDocument implements Action {
	Screen screen;
	Editor editor;
	Document document;
	
	public SwitchToDocument(Screen screen,
				Editor editor,
				Document document) {
	    this.screen = screen;
	    this.editor = editor;
	    this.document = document;
	}
	
	@Override
	public void perform(byte finger, float x, float y) {
	    screen.layers.clear();
	    editor.previousDocument.put(document, editor.document);
	    editor.switchToDocument(document);
	}
	
    }
    
    Screen screen;
    Editor editor;
    public ShowOpenedDocuments(Screen screen, Editor editor) {
	this.screen = screen;
	this.editor = editor;
    }

    @Override
    public void perform(byte finger, float x, float y) {
	x = screen.x[finger];
	y = screen.y[finger];
	//screen.layers.removeLast();
	List<Document> opened = Document.openedDocuments;
	Button [] documents = new Button[opened.size()];
	    
	for (int i = 0; i < opened.size(); ++i) {
	    Document doc = opened.get(i);
	    documents[i] =
		new Button(doc.file.getName(),
			   new SwitchToDocument(screen,
						editor,
						doc));
	}

	Popup popup = new Popup(new Below(documents));
	popup.centerAround(x, y,
			   screen.width,
			   screen.height);

	screen.layers.addLast(popup);
    }
}
