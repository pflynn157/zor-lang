#!/bin/bash

echo ""
echo "Running AST test..."
echo ""

for f in test/ast/*
do
    echo `basename $f .tl`

    NAME=`basename $f`
    build/main --ast test/parser/$NAME > /tmp/$NAME
    
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

