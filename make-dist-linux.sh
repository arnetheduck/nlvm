#!/bin/bash

# Experimental release script

set -e

ROOT=nlvm-linux-$(git rev-parse --short HEAD)

rm -rf $ROOT

# Make sure the nlvm binary is fresh
rm -f nlvm/nlvmr
make STATIC_LLVM=1 nlvm/nlvmr

# Copy nlvm and library files
mkdir -p $ROOT $ROOT/lib
cp nlvm/nlvmr $ROOT/nlvm
strip $ROOT/nlvm

cp -ar lib/nlvm $ROOT/lib/
cp -ar lib/clang $ROOT/lib/

mkdir -p $ROOT/lib/nim
cd lib/nim
# avoid build junk
git archive --format=tar HEAD lib config | (cd ../../$ROOT/lib/nim && tar xf -)
cd ../..

rm -rf dist
mkdir -p dist
tar cvfJ dist/$ROOT.tar.xz $ROOT/

# AppImages have some more requirements - set these up now
cd $ROOT
mv nlvm AppRun
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
