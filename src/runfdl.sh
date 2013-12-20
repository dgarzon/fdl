#!/bin/sh


if [ ! -f "c/libraries/liblist.a" ] || [ ! -f "c/libraries/libpath.a" ] ; then
    cd c/libraries
    make >> lib_msgs.txt
    cd ../..
fi

if [ ! -f "preprocessor/./preprocessor" ]; then
    cd preprocessor
    make >> preproc_msgs.txt
    cd ..
fi

if [ ! -f "./fdl" ]; then
    make >> compiler_msgs.txt
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

	#echo "Compiling '$prepfile'"
	if [ ! -f $prepfile ]; then
		echo "$prepfile does not exist"
        return
	fi
	# converting from fdlp to C
    $FDL $prepfile > "${reffile}.c" #&& echo "Ocaml to C of $1 succeeded"

    # compliling the C file
    if [ -f "${reffile}.c" ]; then
    	gcc -Ic/libraries -Lc/libraries -llist -lpath -w -o "${reffile}" "${reffile}.c" 2>> errors.txt
    else
    	#echo "Ocaml to C of $1 failed"
    	return
    fi

 	# running the binary
    if [ -f "${reffile}" ]; then
        eval ${reffile} 2>> errors.txt
        rm -rf ${reffile}.fdlp
        rm -rf ${reffile}.c
        rm -rf ${reffile}
    #else
        #echo "C to binary of ${reffile}.c failed"
    fi

}

if [ -f $1 ]; then
	compileAndRun $1
else
	echo "$1 doesnt exist"
fi
