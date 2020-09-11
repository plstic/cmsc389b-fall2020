#!/bin/bash

timeout=10 # seconds

cmd=./besbubo
if [ $1 == "pub-hello" ]
then
    cmd=./hello
fi

test=$(timeout $timeout $cmd < tests/$1.input)
if [ $? == 124 ]
then
    echo "timeout err $1"
elif [ -z "$(diff <(echo $test) tests/$1.output)" ]
then
    echo "passed test $1"
else
    echo "failed test $1"
fi
