##      This file is part of JACC and is licenced under terms contained in the COPYING file
##
##      Copyright (C) 2021 Barcelona Supercomputing Center (BSC)

#!/bin/bash

set -e
set -o pipefail

OPENACC=pgcc

JACC_PATH=${HOME}/work/jacc
JACC="gosh ${JACC_PATH}/main.scm"
C2X=${JACC_PATH}/tools/c-to-xcodeml
X2C=${JACC_PATH}/tools/xcodeml-to-c

TMPFILE="a.out tmp.c tmp.xml out.xml out.c jacc.o out.o"

test ()
{
    i=$1
    printf "${i}:\t"
    {
	    pgcc ${i}; ./a.out; rm -f ./a.out;
	    pgcc -acc -ta=tesla ${i}; ./a.out; rm -f ./a.out;

	    for opt in "-acc -ta=tesla" "-acc -ta=multicore" "-ldl"; do
		    cp ${i} tmp.c;

            ${C2X} tmp.c ||
                echo 'C2X Error';
		    ${JACC} < tmp.xml >| out.xml ||
                echo 'JACC Error';
            ${X2C} out.xml ||
                echo 'X2C Error';
		    pgcc -DJACC_OPENACC=${OPENACC} -DJACC_OPENACC_OPTION=${opt} \
			     -lffi ${opt} -I../tools/ ../tools/jacc.c out.c ||
                echo 'Compilation Error';
		    ./a.out ||
                echo 'Execution Error';
	    done;
    } 2>${ERR} | sort | uniq | wc -l |
        sed 's/^1$/Passed/; s/^[0-9].*$/Failed/;'

    rm -f $TMPFILE;
}


if [[ $1 == "-d" ]]; then
    ERR=/dev/stderr
else
    ERR=/dev/null
fi

if [[ -f $1 ]]; then
    test $1
elif [[ -f $2 ]]; then
    test $2
elif [[ $1 == "clean" ]]; then
    rm -f $TMPFILE
else
    for i in $(ls [0-9]*_*.c | sort -n); do
        test $i
    done
fi
