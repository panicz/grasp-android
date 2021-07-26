package com.GRASP;

import android.Manifest;
import android.os.Environment;
import java.io.File;
import android.content.pm.PackageManager;
import android.os.Build;
import java.lang.Math;

import java.util.Arrays;


class OpenFileBrowser implements Action, Procedure {
    Screen screen;
    Editor editor;
    File dir;
    byte finger;
    
    public OpenFileBrowser(Screen screen,
			   Editor editor,
			   File dir) {
 	this.screen = screen;
	this.editor = editor;
	this.dir = dir;
    }
    
    @Override //Procedure
    public void perform() {

	String [] filenames = dir.list();
	File [] files = new File[filenames.length];
	for(int i = 0; i < files.length; ++i) {
	    files[i] = new File(dir, filenames[i]);
	}

	Arrays.sort(files, DirectoriesFirst.instance);
	
	Button [] buttons = new Button[files.length+1];

	buttons[0] = new FileButton(dir.getParentFile(), "..", this);
	
	for (int i = 1; i < buttons.length; ++i) {
	    buttons[i] = new FileButton(files[i-1], this);
	}
	
	Pad list = new Below(buttons);
	Popup popup = new Popup(new Scroll(list,
					   list.width(),
					   Math.min(list.height(),
						    screen.height
						    - 200)))
	    .centerAround(screen.x[finger], screen.y[finger],
			  screen.width, screen.height);
	/*
	popup.centerAround(x, y,
			   screen.width,
			   screen.height);*/

	screen.layers.addLast(popup);
    }
    
    @Override // Action
    public void perform(byte finger, float x, float y) {
	//screen.layers.clear();
	assert(dir.isDirectory());
	this.finger = finger;

	String read_fs = Manifest.permission.READ_EXTERNAL_STORAGE;
	
	if (GRASP.instance.checkSelfPermission(read_fs)
	    == PackageManager.PERMISSION_DENIED) {
	    GRASP.instance.permissionGranted = this;
	    GRASP.instance.requestPermissions(new String [] {
		    read_fs
		}, 1);
	}
	else {
	    /*Procedure.*/perform();
	}
    }
}
