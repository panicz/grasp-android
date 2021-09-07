package com.GRASP;

class NoActionOnPermissionGranted implements PermissionGrantedHandler {
    public static PermissionGrantedHandler instance =
	new NoActionOnPermissionGranted();

    @Override
    public void onPermissionGranted(int requestCode,
				    String[] permissions,
				    int[] grantResults) {}
}
