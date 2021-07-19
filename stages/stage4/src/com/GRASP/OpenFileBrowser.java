package com.GRASP;

import android.Manifest;
import android.os.Environment;
import java.io.File;
import android.content.pm.PackageManager;
import android.os.Build;

class OpenFileBrowser implements Action, Procedure {
    Screen screen;
    Editor editor;
    File dir;

    public OpenFileBrowser(Screen screen,
			   Editor editor,
			   File dir) {
	this.screen = screen;
	this.editor = editor;
	this.dir = dir;
    }

    @Override //Procedure
    public void perform() {
	String [] files = dir.list();
	Button [] buttons = new Button[files.length];

	for (int i = 0; i < files.length; ++i) {
	    buttons[i] = new Button(files[i]);
	}

	Popup popup = new Popup(new Below(buttons));
	/*
	popup.centerAround(x, y,
			   screen.width,
			   screen.height);*/

	screen.layers.addLast(popup);
    }
    
    @Override // Action
    public void perform(byte finger, float x, float y) {
	//screen.layers.clear();
	GRASP.log(GRASP.instance.getFilesDir().toString());
	assert(dir.isDirectory());

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
