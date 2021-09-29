package org.scheme.GRASP;

import java.io.File;

interface FileBrowser  {
    public void fileAction(File file, byte finger, float x, float y);
    public void directoryAction(File file, byte finger, float x, float y);
}
