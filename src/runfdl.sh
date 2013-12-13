#!/bin/sh


if [ ! -f "c/libraries/liblist.a" ] || [ ! -f "c/libraries/libpath.a" ] ; then
    cd c/libraries
    make
    cd ../..
fi

if [ ! -f "preprocessor/./preprocessor" ]; then
    cd preprocessor
    make
    cd ..
fi

if [ ! -f "./fdl" ]; then
    make
fi

# fdl exectutable
FDL="./fdl"
# preprocessor executable
PRE="./preprocessor/preprocessor"

function compileAndRun() {
	basename=`echo $1 | sed 's/.*\\///
                             s/.fdl//'`
	reffile=`echo $1 | sed 's/.fdl$//'`
    prepfile=$reffile'.fdlp'
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/"


	$PRE $1 $prepfile

	echo "Compiling '$prepfile'"
	if [ -f $prepfile ]; then
		echo "$prepfile exists"
	fi
	# converting from fdlp to C
    $FDL $prepfile > "${reffile}.c" && echo "Ocaml to C of $1 succeeded"

    # compliling the C file
    if [ -f "${reffile}.c" ]; then
    	gcc -Ic/libraries -Lc/libraries -llist -lpath -o "${reffile}" "${reffile}.c" && echo "COMPILATION of ${reffile}.c succeeded"
    else
    	echo "Ocaml to C of $1 failed"
    	return
    fi

 	# running the binary
    if [ -f "${reffile}" ]; then
        eval "${reffile}" > ${reffile}.generated.out
        rm -rf ${reffile}.generated.out
        rm -rf ${reffile}.c
        rm -rf ${reffile}
    else
        echo "C to binary of ${reffile}.c failed"
    fi

}

if [ -f $1 ]; then
	compileAndRun $1
else
	echo "$1 doesnt exist"
fi
