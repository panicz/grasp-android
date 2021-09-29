package org.scheme.GRASP;

interface PermissionGrantedHandler {
    public static final int NoRequest = 0;
    
    public static final int ExternalReadFromOpenFileBrowser = 1;
    public static final int ExternalReadFromOpenFileBrowserAlreadyGranted = 2;

    public static final int ExternalReadFromSaveFileBrowser = 3;
    public static final int ExternalReadFromSaveFileBrowserAlreadyGranted = 4;

    public static final int ExternalWriteFromSaveFileBrowser = 5;
    public static final int ExternalWriteFromSaveFileBrowserAlreadyGranted = 6;
    
    void onPermissionGranted(int requestCode,
			     String[] permissions,
			     int[] grantResults);
}
