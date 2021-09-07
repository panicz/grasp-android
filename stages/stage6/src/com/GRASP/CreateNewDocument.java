package com.GRASP;

class CreateNewDocument implements Action {
    Screen screen;
    Editor editor;
    public CreateNewDocument(Screen screen, Editor editor) {
	this.screen = screen;
	this.editor = editor;
    }
	
    @Override
    public void perform(byte finger, float x, float y) {
	screen.layers.clear();
	editor.switchToDocument(Document.createNew());
    }	
}
