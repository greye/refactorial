#!/bin/sh
for file in $(ls *.cpp.in)
do
    cp $file $(basename -s .in $file)
done
refactorial -rulespec=$1 *.cpp
