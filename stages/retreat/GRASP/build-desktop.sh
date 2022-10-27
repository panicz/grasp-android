#!/bin/sh

mkdir -p build/desktop
cd src
java -cp "../libs/kawa.jar" kawa.repl \
     -d ../build/desktop -C \
     `java -jar ../libs/kawa.jar --no-warn-unreachable \
     	   -f analdep.scm -- --list grasp-desktop.scm` \
     grasp-desktop.scm
cd ..

cp -r assets build/desktop
cp libs/kawa.jar build/desktop
cd build/desktop
unzip kawa.jar
rm kawa.jar
jar --verbose --create --file ../grasp-desktop.jar \
    --main-class=grasp\$Mndesktop `find ./ -name '*.class'` \
    assets
cd ..
rm -r desktop
