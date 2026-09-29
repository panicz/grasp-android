#!/usr/bin/env sh
# Copyright 2019 (c) all rights reserved
# by BuildAPKs https://buildapks.github.io/buildAPKs/
# Contributeur : https://github.com/HemanthJabalpuri
# Invocation : $HOME/buildAPKs/scripts/sh/build/build.sh
#####################################################################
set -e
[ -z "${RDR:-}" ] && RDR=".." # "$HOME/buildAPKs"
for CMD in aapt apksigner d8 ecj
do
       	[ -z "$(command -v "$CMD")" ] && printf "%s\\n" " \"$CMD\" not found" && NOTFOUND=1
done
[ "$NOTFOUND" = "1" ] && exit

# android.jar (with resources.arsc) for aapt, ecj and d8
if [ -z "${ANDROID_JAR:-}" ]
then
	for JAR in /data/data/com.termux/files/usr/share/java/android.jar \
		   "$HOME/grasp/tools/android.jar"
	do
		[ -f "$JAR" ] && ANDROID_JAR="$JAR" && break
	done
fi
[ -f "${ANDROID_JAR:-}" ] || { printf "%s\\n" " android.jar not found (set ANDROID_JAR)"; exit 1; }
ANDROID_JAR="$(realpath "$ANDROID_JAR")"
[ "$1" ] && [ -f "$1/AndroidManifest.xml" ] && cd "$1"
[ -f AndroidManifest.xml ] || exit

_CLEANUP_() {
       	printf "\\n\\n%s\\n" "Completing tasks..."
       	[ "$CLEAN" = "1" ] && mv "bin/$PKGNAME.apk" .
      	rmdir assets 2>/dev/null ||:
       	rmdir res 2>/dev/null ||:
       	rm -rf bin
       	rm -rf gen
       	rm -rf obj
	printf "\\n\\n%s\\n\\n" "Share https://wiki.termux.com/wiki/Development everwhere🌎🌍🌏🌐!"
}

_UNTP_() {
       	printf "\\n\\n%s\\n\\n\\n""Unable to process"
       	_CLEANUP_
       	exit
}

PKGNAME="$(grep -o "package=.*" AndroidManifest.xml | cut -d\" -f2)"


printf "%s\\n" "Beginning build"
[ -d assets ] || mkdir assets
[ -d res ] || mkdir res
mkdir -p bin
mkdir -p gen
mkdir -p obj


printf "%s\\n" "aapt: started..."
aapt package -f -m \
       	-I "$ANDROID_JAR" \
       	-M "AndroidManifest.xml" \
       	-J "gen" \
       	-S "res" || _UNTP_
printf "%s\\n\\n" "aapt: done"


printf "%s\\n" "ecj: begun..."
for JAVAFILE in $(find ./src/ -type f -name "*.java")
do
       	JAVAFILES="$JAVAFILES $JAVAFILE"
done

for JARFILE in $(find ./lib/ -type f -name "*.jar")
do
    CLASSFILES="$CLASSFILES -classpath $JARFILE"
    JARFILES="$JARFILES $JARFILE"
done



ecj -bootclasspath "$ANDROID_JAR" -d obj -sourcepath . $JAVAFILES $CLASSFILES -source 1.6 -target 1.6 -proc:none || _UNTP_
printf "%s\\n\\n" "ecj: done"


printf "%s\\n" "d8: started..."
d8 --min-api 14 --lib "$ANDROID_JAR" --output bin $(find obj -name "*.class") $JARFILES || _UNTP_
printf "%s\\n\\n" "d8: done"


printf "%s\\n" "Making $PKGNAME.apk..."
aapt package -f \
       	-I "$ANDROID_JAR" \
       	--min-sdk-version 1 \
       	--target-sdk-version 23 \
       	-M AndroidManifest.xml \
       	-S res \
       	-A assets \
       	-F bin/"$PKGNAME.apk" || _UNTP_


printf "\n%s\\n" "Adding classes.dex to $PKGNAME.apk..."
cd bin || _UNTP_
aapt add -f "$PKGNAME.apk" classes.dex || { cd ..; _UNTP_; }

# zipalign must happen before signing (v2+ signatures cover the whole file)
if [ -n "$(command -v zipalign)" ]
then
	zipalign -f -p 4 "$PKGNAME.apk" "$PKGNAME.aligned.apk" || { cd ..; _UNTP_; }
	mv "$PKGNAME.aligned.apk" "$PKGNAME.apk"
fi

printf "\n%s" "Signing $PKGNAME.apk: "
apksigner sign --cert "$RDR/opt/key/certificate.pem" --key "$RDR/opt/key/key.pk8" "$PKGNAME.apk" || { cd ..; _UNTP_; }
printf "%s\\n" "DONE"
printf "%s" "Verifying $PKGNAME.apk: "
apksigner verify --verbose "$PKGNAME.apk" || { cd ..; _UNTP_; }
printf "%s\\n" "DONE"

if [ -d "$HOME/storage/downloads" ];
then
    echo "Copying $PKGNAME.apk to $HOME/storage/downloads/GRASP/"
    mkdir -p "$HOME/storage/downloads/GRASP"
    cp "$PKGNAME.apk" "$HOME/storage/downloads/GRASP/"
    cp "../assets/example.grasp" "$HOME/storage/downloads/GRASP/"
fi

cd ..

CLEAN=1
_CLEANUP_
# build.sh EOF
