#!/bin/bash

test_count=0
OCC="build/src/tlc"

function run_test() {
    for entry in $1
    do
    	name=`basename $entry .tl`
        
        if [[ $3 == "error" ]] ; then
            if [ -f ./ERROR_TEST.sh ] ; then
                rm ERROR_TEST.sh
            fi
            
            echo "#!/bin/bash" >> ERROR_TEST.sh
            echo "$OCC $entry --use-c" >> ERROR_TEST.sh
            chmod 777 ERROR_TEST.sh
            ./test.py $entry  ./ERROR_TEST.sh "error"
            
            if [[ $? != 0 ]] ; then
                rm ERROR_TEST.sh
                exit 1
            fi
            
            rm ERROR_TEST.sh
        else
            if [[ $2 == "sys" ]] ; then
                $OCC $entry $3 -o $name
            elif [[ $2 == "sys2" ]] ; then
                $OCC $entry $3 -o $name --no-start
            elif [[ $2 == "clib" ]] ; then
                $OCC $entry --use-c $3 -o $name
            fi
        
    	    ./test.py $entry ./$name ""
    	    
    	    if [[ $? != 0 ]] ; then
        		exit 1
        	fi
        	
        	rm ./$name
        	rm /tmp/$name.o
        	rm /tmp/$name.asm
    	fi
    	
    	test_count=$((test_count+1))
    done
}

flags=""

echo "Running all tests..."
echo ""

if [[ $1 == "llir" ]] ; then
    OCC="build/src/tlc2"
fi

run_test 'test/basic/*.tl' 'sys' $flags
run_test 'test/syntax/*.tl' 'sys' $flags
run_test 'test/cond/*.tl' 'sys' $flags
run_test 'test/loop/*.tl' 'sys' $flags
run_test 'test/array/*.tl' 'sys' $flags
run_test 'test/func/*.tl' 'sys' $flags
run_test 'test/str/*.tl' 'sys' $flags
run_test 'test/struct/*.tl' 'sys' $flags

echo ""
echo "$test_count tests passed successfully."
echo "Done"

