#!/usr/bin/env sh
set -x
PKGNAME="$(grep -o "package=.*" AndroidManifest.xml | cut -d\" -f2)"


[ -d assets ] || mkdir assets
[ -d res ] || mkdir res
mkdir -p bin
mkdir -p gen
mkdir -p obj

ANDROID_JAR='/data/data/com.termux/files/usr/share/java/android.jar'
KAWA_JAR='libs/kawa.jar'


aapt package -f -m \
	-M "AndroidManifest.xml" \
       	-J "gen" \
       	-S "res"


for SCMFILE in $(find ./src/ -type f -name "*.scm")
do
       	SCMFILES="$SCMFILES $SCMFILE"
done

java -cp $KAWA_JAR:$ANDROID_JAR kawa.repl -d obj -P $PKGNAME.  -T $PKGNAME.hello -C $SCMFILES


dx --dex --min-sdk-version=24 \
   --output=bin/classes.dex obj #$KAWA_JAR 

aapt package -f \
       	-M AndroidManifest.xml \
       	-S res \
       	-A assets \
       	-F bin/"$PKGNAME.apk"

cd bin

aapt  add -f "$PKGNAME.apk" classes.dex

zipalign -p 4 "$PKGNAME.apk" "aligned-$PKGNAME.apk"
mv "aligned-$PKGNAME.apk" "$PKGNAME.apk"

if [ ! -s ~/pland.keystore ]; then
    keytool -genkey -v -keystore ~/pland.keystore -alias pland -keyalg RSA -keysize 2048 -validity 10000
fi
    
jarsigner -storepass quack01 -verbose -sigalg SHA1withRSA -digestalg SHA1 -keystore ~/pland.keystore "$PKGNAME.apk" pland

mv "$PKGNAME.apk" ..

cd ..

if [ -d "$HOME/storage/downloads" ];
then
    echo "Copying $PKGNAME.apk to $HOME/storage/downloads/GRASP/"
    mkdir -p "$HOME/storage/downloads/GRASP"
    cp "$PKGNAME.apk" "$HOME/storage/downloads/GRASP/"
fi

#rm -rf bin/ obj/ gen/
