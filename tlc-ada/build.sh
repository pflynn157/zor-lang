#!/bin/bash

if [ ! -d ./build ]
then
    mkdir build
fi

gprbuild -p tinylang.gpr

