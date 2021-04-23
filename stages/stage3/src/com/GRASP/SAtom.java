package com.GRASP;

import java.lang.StringBuilder;


class SAtom extends SExp {
    public String name;

    public SAtom(String initial_whitespace,
		 String value) {
	preceding_whitespace = initial_whitespace;
	name = value;
    }
    
    @Override
    protected StringBuilder buildString(StringBuilder result) {
	result.append(name);
	return result;
    }

    @Override
    public String toString() {
	return name;
    }

    @Override
    public Bit toBit() {
	return new Atom(name);
    }

}
