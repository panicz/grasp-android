package com.GRASP;

import android.Manifest;
import android.os.Environment;
import java.io.File;
import android.content.pm.PackageManager;
import android.os.Build;
import java.lang.Math;

import java.util.Arrays;


class SaveFileBrowser implements Action {

    Screen screen;
    Editor editor;
    File dir;
    byte finger;
    
    public SaveFileBrowser(Screen screen,
			   Editor editor,
			   File dir) {
 	this.screen = screen;
	this.editor = editor;
	this.dir = dir;
    }
    
    @Override // Action
    public void perform(byte finger, float x, float y) {
	screen.layers
	    .addLast(new
		     Popup(new
			   Scroll(new TextInput(320, "filename", 72, GRASP.strings_font),
				  320, 72)));	
    }

}
