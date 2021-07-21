package com.GRASP;

import android.graphics.Canvas;
import android.graphics.Paint;

import java.lang.Math;
import java.io.File;

class FileButton extends Button {
    File file;
    static final float icon_width = 64;
    
    public FileButton(File dir, String filename) {
	super(filename);
	/*
	    Math.max(GRASP.empty_file_icon
		     .getDocumentAspectRatio() * 64,
		     Math.max(GRASP.file_icon
			      .getDocumentAspectRatio() * 64,
			      GRASP.directory_icon
			      .getDocumentAspectRatio() * 64));*/
	_width += 64 + 8;
	file = new File(dir, filename);
    }

    @Override
    public void renderCaption(Canvas canvas) {
	canvas.translate(8, 16);
	if (file.isDirectory()) {
	    GRASP.directory_icon.renderToCanvas(canvas);
	}
	else {
	    GRASP.file_icon.renderToCanvas(canvas);
	}
	canvas.translate(icon_width, -16);
	super.renderCaption(canvas);
	canvas.translate(-icon_width-8, 0);
    }
    
    
}

