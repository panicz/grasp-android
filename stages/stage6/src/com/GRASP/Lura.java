package com.GRASP;

import gnu.expr.Language;
import kawa.standard.Scheme;
//import gnu.mapping.Environment;
import gnu.mapping.InPort;


class Lura {
    // extracted from Kawa Shell module

    
    public static Language scheme = Scheme.instance;

    public static Lura instance = new Lura();
    
    public Lura() {
	if (scheme == null) {scheme = new Scheme();}
	Scheme.registerEnvironment();
	Environment.setCurrent(scheme.getEnvironment());
    }

    static Object eval(Bit expr, Editor editor) {
	if (instance != null) {
	    return instance.evalBit(expr, editor);
	}
	return null;
    }
    
    Object evalBit(Bit expr, Editor editor) {
	StringBuilder sb = new StringBuilder();
	expr.buildString(sb);
	CharSequenceReader in = new CharSequenceReader(sb);
	InPort inp = new InPort(in);
	Environment env = Environment.getCurrent();
	try {
	    Object result = Scheme.eval(inp, env);
	    GRASP.log(result.getClass().toString()+": "+result.toString());
	}
	catch(Throwable e) {
	    GRASP.log(e.toString());

	//
	
	return null;
    }
}
