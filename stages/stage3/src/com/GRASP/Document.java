package com.GRASP;
import java.util.List;
import java.util.ArrayList;

class Document implements Operations {
    public String path = null;
    public static List<Document> openedDocuments =
	new ArrayList<Document>();

    public Bit root;
    
    public Document() {
	openedDocuments.add(this);
	
    }
    
}
