package com.GRASP;

import android.Manifest;
import android.os.Environment;
import java.io.File;
import java.io.FileWriter;
import java.io.BufferedWriter;
import java.io.IOException;
import android.content.pm.PackageManager;
import android.os.Build;
import java.lang.Math;

import java.util.Arrays;


class SaveFileBrowser implements Action, PermissionGrantedHandler, FileBrowser {
    Screen screen;
    Editor editor;
    File dir;
    File file = null;
    TextInput filename;
    float x0, y0;
    
    public SaveFileBrowser(Screen screen,
			   Editor editor,
			   File dir) {
 	this.screen = screen;
	this.editor = editor;
	this.dir = dir;
    }

    class SaveFileAction implements Action {
	SaveFileBrowser parent;
	
	public SaveFileAction(SaveFileBrowser parent) {
	    this.parent = parent;
	}
	
	@Override // Action
	public void perform(byte finger, float x, float y) {
	    parent.saveFile(new File(parent.dir, parent.filename.getText()));
	}
    }
    
    void showBrowserWindow() {
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
	
	filename = new
	    TextInput(text_width, "filename", 76, GRASP.strings_font);

	save.action = new SaveFileAction(this);
	
	Scroll textfield = new Scroll(filename);

	Beside header = new Beside(textfield, save);

	filelist.trySetSize(header.width(), filelist.height());
	
	Popup popup = new
	    Popup(new
		  Below(header,
			new Scroll(filelist,
				   header.width(),
				   Math.min(header.height() + filelist.height(),
					    screen.height - 200))))
	    .centerAround(x0, y0, screen.width, screen.height);

	screen.layers.addLast(popup);
    }


    void saveFile(File file) {
	this.file = file;
	String write_fs = Manifest.permission.WRITE_EXTERNAL_STORAGE;
	
	if (GRASP.instance.checkSelfPermission(write_fs)
	    == PackageManager.PERMISSION_DENIED) {
	    GRASP.instance.permissionGranted = this;
	    GRASP.instance.requestPermissions(new String [] {
		    write_fs
		}, ExternalWriteFromSaveFileBrowser);
	}
	else {
	    onPermissionGranted(ExternalWriteFromSaveFileBrowserAlreadyGranted,
				null, null);
	}
    }

    void saveFile() {
	BufferedWriter writer = null;
	try {
	    writer = new BufferedWriter(new FileWriter(file));
	    writer.append(editor.document.buildString(new StringBuilder()));
	    writer.close();
	} catch(IOException but) {
	    GRASP.log(but.toString());
	}
    }
    
    @Override
    public void onPermissionGranted(int requestCode,
				    String[] permissions,
				    int[] grantResults) {
	switch(requestCode) {
	case ExternalReadFromSaveFileBrowser:
	case ExternalReadFromSaveFileBrowserAlreadyGranted:
	    showBrowserWindow();
	    break;
	case ExternalWriteFromSaveFileBrowser:
	case ExternalWriteFromSaveFileBrowserAlreadyGranted:
	    saveFile();
	    break;
	default:
	    GRASP.log("unsupported request code: "+requestCode);
	    break;
	}
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
		}, ExternalReadFromSaveFileBrowser);
	}
	else {
	    onPermissionGranted(ExternalReadFromSaveFileBrowserAlreadyGranted,
				null, null);
	}
    }

    @Override
    public void fileAction(File file, byte finger, float x, float y) {
	filename.setText(file.getName());
    }

    @Override
    public void directoryAction(File file, byte finger, float x, float y) {
	dir = file;
	screen.layers.removeLast();
	perform(finger, x, y);
    }

}
