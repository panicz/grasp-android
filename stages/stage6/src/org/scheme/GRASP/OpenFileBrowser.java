package org.scheme.GRASP;

import android.Manifest;
import android.os.Environment;
import java.io.File;
import android.content.pm.PackageManager;
import android.os.Build;
import java.lang.Math;

import java.util.Arrays;


class OpenFileBrowser implements Action, PermissionGrantedHandler, FileBrowser {
    Screen screen;
    Editor editor;
    File dir;
    float x0, y0;
    
    public OpenFileBrowser(Screen screen,
			   Editor editor,
			   File dir) {
 	this.screen = screen;
	this.editor = editor;
	this.dir = dir;
    }
    
    @Override
    public void onPermissionGranted(int requestCode,
				    String[] permissions,
				    int[] grantResults) {

	String [] filenames = dir.list();
	File [] files = new File[filenames.length];
	for(int i = 0; i < files.length; ++i) {
	    files[i] = new File(dir, filenames[i]);
	}

	Arrays.sort(files, DirectoriesFirst.comparator);
	
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
	    .centerAround(x0, y0, screen.width, screen.height);
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
	x0 = screen.x[finger];
	y0 = screen.y[finger];

	String read_fs = Manifest.permission.READ_EXTERNAL_STORAGE;
	
	if (GRASP.instance.checkSelfPermission(read_fs)
	    == PackageManager.PERMISSION_DENIED) {
	    GRASP.instance.permissionGranted = this;
	    GRASP.instance.requestPermissions(new String [] {
		    read_fs
		}, ExternalReadFromOpenFileBrowser);
	}
	else {
	    onPermissionGranted(ExternalReadFromOpenFileBrowserAlreadyGranted,
				null, null);
	}
    }

    @Override
    public void fileAction(File file, byte finger, float x, float y) {
	Document document = Document.fromFile(file);
	if (document != null) {
	    screen.layers.clear();
	    editor.previousDocument
		.put(document, editor.document);
	    editor.switchToDocument(document);
	}
	else {
	    GRASP.log("failed to open "+file.toString());
	}
    }


    @Override
    public void directoryAction(File file, byte finger, float x, float y) {
	dir = file;
	screen.layers.removeLast();
	perform(finger, x, y);
    }

}
