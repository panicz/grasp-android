#!/bin/sh

mkdir -p build/desktop
kawa -d build/desktop -C \
     `./analdep --list grasp-desktop.scm` grasp-desktop.scm
cp -r fonts build/desktop
cp kawa.jar build/desktop
cd build/desktop
unzip kawa.jar
rm kawa.jar
jar --verbose --create --file grasp-desktop.jar \
    --main-class=grasp-desktop `find ./ -name '*.class'` \
    fonts
