#!/bin/sh

FDL="./fdl"
PRE="preprocessor/./preprocessor"

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
	difference=$(diff -b $1 $2)
	echo $difference
	if [ "$difference" != "" ]; then
		echo $difference > $3
	fi
}

function compile() {
	basename=`echo $1 | sed 's/.*\\///
                             s/.fdl//'`
	reffile=`echo $1 | sed 's/.fdl$//'`
    prepfile=${reffile}'.fdlp'
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/"

	# gets the path of the test output file
	testoutput=`echo ${basedir}test_outputs/$basename.c.out`
	# echo $basename
	# echo $reffile
	# echo $basedir
	# echo $testoutput

    echo "Preprocessing '$1'..."
    # converting from fdl to C
    $PRE $1 $prepfile && echo "Preprocessor for $1 succeeded"

	echo "Compiling $prepfile ..."
	# converting from fdl to C
    $FDL $prepfile > "${reffile}.c" && echo "Ocaml to C of $1 succeeded"

    # compliling the C file
    if [ -f "${reffile}.c" ]; then
    	gcc -Ic/libraries -Lc/libraries -llist -lpath "${reffile}.c" -o "${reffile}" && echo "COMPILATION of ${reffile}.c succeeded"
    else
    	echo "Ocaml to C of $1 failed"
    	return
    fi

    rm -rf $prepfile

	# running the binary
    if [ -f "${reffile}" ]; then
    	eval "${reffile}" > ${reffile}.generated.out
        cp ${reffile}.generated.out ${basedir}test_outputs/$basename.c.out
    	Compare ${testoutput} ${reffile}.generated.out ${reffile}.c.diff
        rm -rf ${basedir}test_outputs/$basename.c.out
        rm -rf ${reffile}.generated.out
        rm -rf ${reffile}.c
    else
    	echo "C to binary of ${reffile}.c failed"
    fi
}

files=sample_program/*.fdl

for file in $files
do
	compile $file
done