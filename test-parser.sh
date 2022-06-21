#!/bin/bash

echo ""
echo "Running parser test..."
echo ""

for f in test/parser/*
do
    echo `basename $f .tl`

    NAME=`basename $f`
    build/main $f > /tmp/$NAME
    
    EXPECTED=`cat $f`
    ACTUAL=`cat /tmp/$NAME`
    diff -wB $f /tmp/$NAME
    
    if [[ $? == 0 ]] ; then
        echo "Pass"
        echo ""
    else
        echo $?
        echo "Fail"
        echo ""
        
        echo "Expected:"
        echo $EXPECTED
        echo ""
        
        echo "Actual:"
        echo $ACTUAL
        echo ""
        
        exit 1
    fi
done

echo ""
echo "Done"
echo ""

