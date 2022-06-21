#!/bin/bash

echo ""
echo "Running C++ unwriter test..."
echo ""

for f in test/cpp/*
do
    echo `basename $f .tl`

    NAME=`basename $f`
    build/main $f --output:cpp > /tmp/$NAME.cpp
    g++ /tmp/$NAME.cpp -o /tmp/$NAME.bin
    
    if [ -f /tmp/$NAME.bin ] ; then
        echo "Pass"
        echo ""
    else
        echo "Fail"
        echo ""
        
        cat $f
        echo "==================="
        echo /tmp/$NAME.cpp
        echo "-------------------"
        echo ""
        echo "C++ unwriter test failed."
        echo "Failed compilation."
        echo ""
        
        exit 1
    fi
done

echo ""
echo "C++ Unwriter Test"
echo "Done"
echo ""

