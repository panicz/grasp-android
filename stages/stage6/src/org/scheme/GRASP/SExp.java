package org.scheme.GRASP;

import java.lang.StringBuilder;

abstract class SExp {
    public String preceding_whitespace;
    //public abstract int width();
    //public abstract int height();
    protected abstract StringBuilder buildString(StringBuilder sb);
    public abstract Bit toBit();
}
