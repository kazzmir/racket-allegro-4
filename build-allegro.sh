#!/bin/sh

allegro=allegro-4.2.0
libpng=libpng-1.2.5

cp hack/loadpng.c $allegro/src
cp hack/loadpng.h $allegro/include
cp hack/makefile.in $allegro
cp hack/makefile.lst $allegro

cd $libpng
make -f scripts/makefile.linux
cd ..
cp $libpng/libpng12.so $allegro/lib/unix
cp $libpng/png.h $allegro/include
cp $libpng/pngconf.h $allegro/include

cd $allegro
chmod +x misc/deplib.sh misc/depmod.sh misc/deplexe.sh misc/depdlib.sh misc/depdexe.sh misc/depmexe.sh
sh configure --disable-asm
make lib
cp modules.lst lib/unix
