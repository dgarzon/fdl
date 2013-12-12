#!/bin/sh

# fdl exectutable
FDL="./fdl"
# preprocessor executable
PREPROCESSOR="./preprocessor/preprocessor"

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
	difference=$(diff -b $1 $2) 
	echo $difference
	if [ "$difference" != "" ]; then
		echo $difference > $3	
	fi
}

function compileAndRun() {
	basename=`echo $1 | sed 's/.*\\///
                             s/.fdl//'`
	reffile=`echo $1 | sed 's/.fdl$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/"
	
	# gets the path of the test output file
	# testoutput=`echo ${basedir}test_outputs/$basename.c.out`
	# echo $basename
	# echo $reffile
	# echo $basedir
	# echo $testoutput

	preprocessorOutputFileName=$basename".fdlp"
	# echo $preprocessorOutputFileName

	$PREPROCESSOR $1 $preprocessorOutputFileName

	echo "Compiling '$preprocessorOutputFileName'"
	if [ -f $preprocessorOutputFileName ]; then
		echo "$preprocessorOutputFileName exists"
	fi
	# converting from fdl to C
    $FDL $preprocessorOutputFileName > "${reffile}.c" && echo "Ocaml to C of $1 succeeded"

    # compliling the C file
    if [ -f "${reffile}.c" ]; then
    	gcc -Ic/libraries -Lc/libraries -llist -lpath -o "${reffile}" "${reffile}.c" && echo "COMPILATION of ${reffile}.c succeeded"
    else
    	echo "Ocaml to C of $1 failed"
    	return
    fi

    # running the binary
    if [ -f "${reffile}" ]; then
    	eval "${reffile}"
    	#rm -rf $preprocessorOutputFileName
    	#rm -rf ${reffile}.c
    	rm -rf ${reffile}
    else
    	echo "Could not run the C program at ${reffile}"
    fi


	# # running the binary
 #    if [ -f "${reffile}" ]; then
 #    	eval "${reffile}" > ${reffile}.generated.out
 #    	Compare ${testoutput} ${reffile}.generated.out ${reffile}.c.diff
 #    else
 #    	echo "C to binary of ${reffile}.c failed"
 #    fi
}

# the path to the .fdl file
if [ -f $1 ]; then
	compileAndRun $1
else
	echo "$1 doesnt exist"
fi


# for file in $files
# do
# 	compile $file
# done
