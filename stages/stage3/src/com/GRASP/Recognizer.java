package com.GRASP;

import android.content.Context;

import java.util.List;
import java.util.ArrayList;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.SecurityException;

class Recognizer {
    public class Rank {
	public String name;
	public float certainty; // between 0 and 1
	public Rank(String name, float certainty) {
	    this.name = name;
	    this.certainty = certainty;
	}
    };

    Context context;

    File gestures_dir;
    
    class StoreSymbolAction implements Action {
	List<Shape> shape;
	String name;
	Recognizer parent;
	public StoreSymbolAction(String name,
				 List<Shape> shape,
				 Recognizer parent) {
	    this.name = name;
	    this.shape = shape;
	    this.parent = parent;
	}

	public PopUp perform() {
	    File path = new File(parent.gestures_dir, name);
	    FileOutputStream out = null;

	    try {
		out = new FileOutputStream(path,
					   /*append*/true);
	    }
	    catch(FileNotFoundException e) {
		GRASP.log(e.toString());
	    }
	    catch(SecurityException s) {
		GRASP.log(s.toString());
	    }
	    finally {
		try {
		    if (out != null) {
			out.close();
		    }
		}
		catch(IOException e) {
		    GRASP.log(e.toString());
		}
	    }

	    return null;
	}
    };

    public Recognizer(Context context) {
	this.context = context;
	gestures_dir =
	    context.getDir("gestures", Context.MODE_PRIVATE);
    }
    
    public List<Rank> candidates(List<Shape> shape) {
	// powinnismy przekonwertowac shape
	// do postaci kwadratu pikseli
	// i wywolac na tym kwadracie siec
	// neuronowa, a nastepnie zwrocic
	// neurony wyjsciowe z najwyzszymi
	// pobudzeniami
	return new ArrayList<Rank>();
    }

    public List<String> known_symbols() {
	// wszystkie podfoldery z katalogu
	// GRASP/shapes
	
	return new ArrayList<String>();
    }

    public Action store(String name, List<Shape> shape) {
	return new StoreSymbolAction(name, shape, this);
    }
}
