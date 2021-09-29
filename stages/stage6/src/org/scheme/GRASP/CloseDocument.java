package org.scheme.GRASP;

class CloseDocument implements Action {
    Screen screen;
    Document document;
	
    public CloseDocument(Screen screen,
			 Document document) {
	this.screen = screen;
	this.document = document;
    }
	
    @Override
    public void perform(byte finger, float x, float y) {
	screen.layers.clear();
	screen.closeDocument(document);
    }	
}
