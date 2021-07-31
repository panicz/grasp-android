package com.GRASP;

import android.Manifest;
import android.os.Environment;
import java.io.File;
import android.content.pm.PackageManager;
import android.os.Build;
import java.lang.Math;

import java.util.Arrays;


class SaveFileBrowser implements Action, Procedure, FileBrowser {
    Screen screen;
    Editor editor;
    File dir;
    float x0, y0;
    
    public SaveFileBrowser(Screen screen,
			   Editor editor,
			   File dir) {
 	this.screen = screen;
	this.editor = editor;
	this.dir = dir;
    }
    
    @Override
    public void execute() {
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

	Below filelist = new Below(buttons);

	Button save = new Button("Save");
	
	float text_width = Math.max(420, filelist.width() - save.width());
	
	TextInput filename = new
	    TextInput(text_width, "filename", 72, GRASP.strings_font);

	Scroll textfield = new Scroll(filename, filename.width(), filename.height());

	Beside header = new Beside(textfield, save);
	
	Popup popup = new
	    Popup(new
		  Below(header,
			new Scroll(new Below(buttons),
				   header.width(),
				   Math.min(header.height() + filelist.height(),
					    screen.height - 200))))
	    .centerAround(x0, y0, screen.width, screen.height);

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
		}, 1);
	}
	else {
	    /*Procedure.*/execute();
	}
    }

    @Override
    public void fileAction(File file, byte finger, float x, float y) {

    }

    @Override
    public void directoryAction(File file, byte finger, float x, float y) {

    }

}
