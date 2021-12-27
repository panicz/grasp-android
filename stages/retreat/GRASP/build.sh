#!/bin/sh
set -x

SOURCES=$(cat sources.list)

ANDROID=$(locate android.jar | tail -n 1)


cd src

if [ $# -gt 0 ]; then
    KAWA=$1
else
    KAWA="java -cp $ANDROID:../libs/kawa.jar kawa.repl"
fi

VERSION=$($KAWA --version 2>&1| head -n 1 |sed 's/^Kawa \([^ ]*\).*/\1/')
BUILDDIR="build-$VERSION"

$KAWA -C $SOURCES
mkdir -p ../$BUILDDIR/
mv *.class ../$BUILDDIR/
cd ..
