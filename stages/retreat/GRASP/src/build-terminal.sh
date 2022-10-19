#!/bin/sh

mkdir -p build/terminal
CLASSPATH=./lanterna-3.1.1.jar kawa -d build/terminal -C \
     `./analdep --list grasp-terminal.scm` grasp-terminal.scm
cp lanterna-3.1.1.jar build/terminal
cp kawa.jar build/terminal
cd build/terminal
unzip lanterna-3.1.1.jar
unzip -uo kawa.jar
rm lanterna-3.1.1.jar
rm kawa.jar
jar --verbose --create --file grasp-terminal.jar \
    --main-class=grasp-terminal `find ./ -name '*.class'`

