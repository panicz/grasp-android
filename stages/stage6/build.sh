#!/usr/bin/env bash
# Copyright 2019 (c) all rights reserved
# by BuildAPKs https://buildapks.github.io/buildAPKs/
# Contributeur : https://github.com/HemanthJabalpuri
# Invocation : $HOME/buildAPKs/scripts/sh/build/build.sh
#####################################################################
set -x
[ -z "${RDR:-}" ] && RDR=".." # "$HOME/buildAPKs"
for CMD in aapt apksigner d8 ecj
do
       	[ -z "$(command -v "$CMD")" ] && printf "%s\\n" " \"$CMD\" not found" && NOTFOUND=1
done
[ "$NOTFOUND" = "1" ] && exit
[ "$1" ] && [ -f "$1/AndroidManifest.xml" ] && cd "$1"
[ -f AndroidManifest.xml ] || exit

_CLEANUP_() {
       	[ "$CLEAN" = "1" ] && mv "bin/$PKGNAME.apk" .
      	rmdir assets 2>/dev/null ||:
       	rmdir res 2>/dev/null ||:
       	rm -rf bin
       	rm -rf gen
       	#rm -rf obj
	printf "\\n\\n%s\\n\\n" "Share https://wiki.termux.com/wiki/Development everwhere🌎🌍🌏🌐!"
}

_UNTP_() {
       	printf "\\n\\n%s\\n\\n\\n""Unable to process"
       	_CLEANUP_
       	exit
}

PKGNAME="$(grep -o "package=.*" AndroidManifest.xml | cut -d\" -f2)"
ANDROID_JAR='/data/data/com.termux/files/usr/share/java/android.jar'
KAWA_JAR=lib/kawa-1.13.jar

rm -rf obj
[ -d assets ] || mkdir assets
[ -d res ] || mkdir res
mkdir -p bin
mkdir -p gen
mkdir -p obj



aapt package -f -m \
       	-M "AndroidManifest.xml" \
       	-J "gen" \
       	-S "res" || _UNTP_


for SCMFILE in $(find ./src/ -type f -name "*.scm")
do
    java -cp $KAWA_JAR:$ANDROID_JAR kawa.repl -d obj \
	 -P $(dirname $SCMFILE | sed 's/.*src\///' | tr / .). \
	 --module-static -C $SCMFILE
done


set +x
for JAVAFILE in $(find ./src/ -type f -name "*.java")
do
       	JAVAFILES="$JAVAFILES $JAVAFILE"
done

CLASSFILES=obj

for JARFILE in $(find ./lib/ -type f -name "*.jar")
do
    CLASSFILES="$CLASSFILES:$JARFILE"
    JARFILES="$JARFILES $JARFILE"
done
set -x


ecj -d obj -sourcepath . $JAVAFILES -classpath $CLASSFILES -source 1.5 -target 1.5 || _UNTP_


d8 --lib $ANDROID_JAR `find obj -name '*.class'`  `find lib -name '*.dex'` || _UNTP_

aapt package -f \
       	--min-sdk-version 1 \
       	--target-sdk-version 23 \
       	-M AndroidManifest.xml \
       	-S res \
       	-A assets \
       	-F bin/"$PKGNAME.apk" || _UNTP_

mv classes.dex bin/
cd bin || _UNTP_
aapt add -f "$PKGNAME.apk" classes.dex || { cd ..; _UNTP_; }


apksigner sign --cert "$RDR/opt/key/certificate.pem" --key "$RDR/opt/key/key.pk8" "$PKGNAME.apk" || { cd ..; _UNTP_; }

apksigner verify --verbose "$PKGNAME.apk" || { cd ..; _UNTP_; }


if [ -d "$HOME/storage/downloads" ];
then
    mkdir -p "$HOME/storage/downloads/GRASP"
    cp "$PKGNAME.apk" "$HOME/storage/downloads/GRASP/"
    cp "../assets/example.grasp" "$HOME/storage/downloads/GRASP/"
fi

cd ..

CLEAN=1
_CLEANUP_
# build.sh EOF
