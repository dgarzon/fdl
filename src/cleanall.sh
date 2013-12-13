#!/bin/sh

if [ -f "c/libraries/liblist.a" ] || [ -f "c/libraries/libpath.a" ] ; then
    cd c/libraries
    make clean
    cd ../..
fi

if [ -f "preprocessor/./preprocessor" ]; then
    cd preprocessor
    make clean
    cd ..
fi

if [ -f "./fdl" ]; then
    make clean
fi