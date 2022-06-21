#!/bin/bash

echo ""
echo "Running error test..."
echo ""

for f in test/error/*.tl
do
    echo `basename $f .tl`

    NAME=`basename $f .tl`
    build/main test/error/$NAME.tl --testing > /tmp/$NAME
    
    EXPECTED=`cat test/error/$NAME.out`
    ACTUAL=`cat /tmp/$NAME`
    diff -wB test/error/$NAME.out /tmp/$NAME
    
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

