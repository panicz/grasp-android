package com.GRASP;
import java.util.List;
import java.util.ArrayList;

class Document {
    public String path = null;
    public static List<Document> openedDocuments =
	new ArrayList<Document>();
    public Document() {
	openedDocuments.add(this);
    }
    
}
