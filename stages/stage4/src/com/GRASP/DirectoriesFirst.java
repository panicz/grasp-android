package com.GRASP;
import java.io.File;
import java.util.Comparator;

class DirectoriesFirst implements Comparator<File> {
		
    @Override
    public int compare(File a, File b) {
	if (a.isDirectory() && !b.isDirectory()) {
	    return -1;
	}

	if (!a.isDirectory() && b.isDirectory()) {
	    return 1;
	}

	return a.compareTo(b);
    }

    public static Comparator<File> comparator = new DirectoriesFirst();
}
