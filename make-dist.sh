#!/bin/bash

# Experimental release script

set -e

ROOT=nlvm-$(git rev-parse --short HEAD)

rm -rf $ROOT

# Make sure the nlvm binary is fresh
rm -f nlvm/nlvmr
make STATIC_LLVM=1 nlvm/nlvmr

# Copy nlvm and library files
# TODO these would go in /usr/{bin, share/Nim} normally
mkdir -p $ROOT
cp nlvm/nlvmr $ROOT/nlvm
cp -r nlvm-lib $ROOT
mkdir -p $ROOT/Nim
cd Nim
# avoid build junk
git archive --format=tar HEAD lib config | (cd ../$ROOT/Nim && tar xf -)
cd ..

rm -rf dist
mkdir -p dist
tar cvfJ dist/$ROOT.tar.xz $ROOT/

# AppImages have some more requirements - set these up now
cd $ROOT
ln -s nlvm AppRun
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
  wget -P ext/ https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage
  chmod +x ext/appimagetool-x86_64.AppImage
}

ext/appimagetool-x86_64.AppImage $ROOT
mv nlvm*.AppImage dist/

rm -rf $ROOT
