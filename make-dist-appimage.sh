#!/bin/bash
# Turn a native nlvm release into an AppImage

ROOT=nlvm-linux-$(git rev-parse --short HEAD)

tar xvf dist/$ROOT-native.tar.xz
cd $ROOT
mv bin/nlvm AppRun
echo "[Desktop Entry]
Name=nlvm
Exec=AppRun
Icon=nlvm
Type=Application
Categories=Development;
" > nlvm.desktop

# TODO
touch nlvm.png

cd ..

mkdir -p ext

[ -f ext/appimagetool-x86_64.AppImage ] || {
  wget -P ext/ https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-x86_64.AppImage
  chmod +x ext/appimagetool-x86_64.AppImage
}

ext/appimagetool-x86_64.AppImage $ROOT
mv nlvm*.AppImage dist/

rm -rf $ROOT
