#!/bin/bash

timeout=10 # seconds

run_test() {
    timeout $timeout $1 < tests/$2.input > tmp.txt
    if [ $? == 124 ]
    then
        echo "timeout err $2" >> output.txt
    elif [ -z "$(diff tmp.txt tests/$2.output)" ]
    then
        echo "passed test $2" >> output.txt
    else
        echo "failed test $2" >> output.txt
    fi
    rm tmp.txt
}

run_test ./hello pub-hello

run_test ./besbubo pub01
run_test ./besbubo pub02
run_test ./besbubo pub03
