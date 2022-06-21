#!/bin/bash

LIB_INSTALL=/usr/local/lib/tinylang
INCLUDE_INSTALL=/usr/local/include/tinylang

if [[ ! -d $LIB_INSTALL ]] ; then
    sudo mkdir -p $LIB_INSTALL
fi

if [[ ! -d $INCLUDE_INSTALL ]] ; then
    sudo mkdir -p $INCLUDE_INSTALL
fi

as lib/x64_start.asm -o build/ti_start.o

sudo cp build/ti_start.o $LIB_INSTALL
sudo cp build/lib/libtinylang.so /usr/lib

sudo cp -r lib/include/* $INCLUDE_INSTALL

sudo ldconfig

echo "Done"

