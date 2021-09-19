package com.GRASP;

import gnu.lists.Pair;
import java.lang.System;

class Cons extends Pair {
    public Cons(Object a, Object d) {
	super(a, d);
    }
    
    @Override
    public boolean equals(Object x) {
	return this == x;
    }

    @Override
    public int hashCode() {
	return System.identityHashCode(this);
    }
}
