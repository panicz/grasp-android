package com.GRASP;

import gnu.expr.Language;
import kawa.standard.Scheme;
import gnu.mapping.Environment;
import gnu.mapping.InPort;
import gnu.mapping.Values;
import gnu.lists.Pair;
import gnu.lists.LList;
import java.lang.Exception;


class Lura {
    
    public static Language scheme = Scheme.instance;

    public static Lura instance = new Lura();
    
    public Lura() {
	if (scheme == null) {scheme = new Scheme();}
	Scheme.registerEnvironment();
	Environment.setCurrent(scheme.getEnvironment());
	try {
	    InPort in = new InPort(GRASP.instance.getAssets()
				   .open("init.scm"));
	    Environment env = Environment.getCurrent();
	    Scheme.eval(in, env);
	} catch(Exception e) {
	    GRASP.log(e.toString());
	}
    }

    static DragAround insert = new
	DragAround(null, 16, 0);
    
    static Bit addBit(Object object, Bit paradigm, Space preceding) {
	if (object == null || object instanceof java.lang.Package) {
	    return null;
	}
	if (object instanceof Values) {
	    Values v = (Values) object;
	    if (v.isEmpty()) {
		return null;
	    }
	    else {
		Object a [] = v.getValues();
		Bit first = null;
		for (int i = a.length-1; i >= 0; --i) {
		    first = addBit(a[i], paradigm, preceding);
		}
		return first;
	    }
	}

	Bit result;
	if (object instanceof Pair) {
	    Box box = new Box();
	    
	    Space tip = box.first_interline
		.following_line.first_space;
	    while (object instanceof Pair) {
		tip = addBit(((Pair)object).getCar(), null, tip)
		    .following_space();
		object = ((Pair)object).getCdr();
	    }
	    if (object != LList.Empty) {
		GRASP.log("need to convert dotted tail");
	    }
	    result = box;
	}
	else if (object == LList.Empty) {
	    result = new Box();
	}
	else {
	    // domyslnie zwracamy atoma
	    GRASP.log(object.getClass().toString());
	    result = new Atom(object.toString());
	}
	insert.target = result;
	preceding.insertAt(0, 0, insert);
	insert.target = null;
	return result;
    }
    
    static Bit eval(Bit expr, Editor editor) {
	if (instance != null) {
	    return instance.evalBit(expr, editor);
	}
	return null;
    }
    
    Bit evalBit(Bit expr, Editor editor) {
	StringBuilder sb = new StringBuilder();
	expr.buildString(sb);
	CharSequenceReader in = new CharSequenceReader(sb);
	InPort inp = new InPort(in);
	Environment env = Environment.getCurrent();
	Object result = null;
	try {
	    result = Scheme.eval(inp, env);
	    Space preceding_space = editor.evaluation_target == editor
		? expr.following_space()
		: expr.following_space();
	    if (preceding_space == null) {
		preceding_space = new Space();
		expr.set_following_space(preceding_space);
	    }
	    
	    addBit(result, expr, preceding_space);
	    editor.document.preserve_distance_between_elements();
	    //GRASP.log(result.getClass().toString()+": "+result.toString());
	}
	catch(Throwable e) {
	    GRASP.log(e.toString());
	}
	return null;
    }
}
