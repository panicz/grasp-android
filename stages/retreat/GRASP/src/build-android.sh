#!/bin/sh

ANDROID_JAR='/data/data/com.termux/files/usr/share/java/android.jar'

mkdir -p build/android/bin
mkdir -p build/android/gen
mkdir -p build/android/obj

PKGNAME="$(grep -o "package=.*" AndroidManifest.xml | cut -d\" -f2)"

aapt package -f -m \
     -M "AndroidManifest.xml" \
     -J "build/android/gen" \
     -S "res"

java -cp ./kawa.jar:$ANDROID_JAR kawa.repl \
     -d build/android/obj -P $PKGNAME. \
     -T $PKGNAME.Grasp -C \
     `./analdep --list grasp-android.scm` \
     grasp-android.scm

d8 --min-api 28 --lib $ANDROID_JAR \
   `find build/android/obj -name '*.class'` kawa.jar

mv classes.dex build/android/bin/

aapt package -f \
       	-M AndroidManifest.xml \
       	-S res \
       	-A assets \
       	-F build/android/bin/"$PKGNAME.apk"

cd build/android/bin

aapt  add -f "$PKGNAME.apk" classes.dex

zipalign -p 4 "$PKGNAME.apk" "aligned-$PKGNAME.apk"
mv "aligned-$PKGNAME.apk" "$PKGNAME.apk"

if [ ! -s ~/pland.keystore ]; then
    keytool -genkey -v -keystore ~/pland.keystore \
	    -alias pland -keyalg RSA -keysize 2048 -validity 10000
fi
    
jarsigner -storepass quack01 -verbose -sigalg SHA1withRSA \
	  -digestalg SHA1 -keystore ~/pland.keystore "$PKGNAME.apk" pland

mv "$PKGNAME.apk" ..

cd ..

if [ -d "$HOME/storage/downloads" ];
then
    echo "Copying $PKGNAME.apk to $HOME/storage/downloads/GRASP/"
    mkdir -p "$HOME/storage/downloads/GRASP"
    cp "$PKGNAME.apk" "$HOME/storage/downloads/GRASP/"
fi
