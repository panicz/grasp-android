package com.GRASP;

import java.util.Collection;
import java.util.Map;
import java.util.HashMap;

class Type {
    Type base;
    String name;

    private Type(Type base, String name) {
	this.base = base;
	this.name = name;
    }
    
    public static Type derive(Type base,
			      String name) {
	Type derived = new Type(base, name);
	byName.put(name, derived);
	return derived;
    }
	
    private static Map<String, Type> byName =
	new HashMap<String, Type>();
    
    public static Type named(String name) {
	return byName.get(name);
    };

    public static Collection<Type> universe() {
	return byName.values();
    }
};
