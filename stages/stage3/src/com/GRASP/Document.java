package com.GRASP;
import java.util.List;
import java.util.ArrayList;

class Document extends Box implements Operations {
    public String path = null;
    public static List<Document> openedDocuments =
	new ArrayList<Document>();
    public Document() {
	openedDocuments.add(this);
    }
    
}
