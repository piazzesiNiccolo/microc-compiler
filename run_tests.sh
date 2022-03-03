#!/bin/bash

clang="clang -w"
microcc="microcc"

for file in test/*.mc; do
    name=${file%%.*}
    echo "Running $name"
    dune exec -- $microcc $name.mc -o $name.bc
    if [ $? -ne 0 ]; then
        break
    fi
    $clang $name.bc src/rt-support.c -o $name.elf
    diff <($name.elf) $name.out
    if [ $? -ne 0 ]; then
        rm -f $name.elf
        break
    fi
    echo "Test passed!"
    rm -f $name.elf
  
done

rm -f test/*.bc
rm -f test/*.elf
