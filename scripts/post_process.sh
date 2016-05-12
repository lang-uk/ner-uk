#!/bin/bash

for file in *.ann
do
    awk 'BEGIN {i = 1} {if ($1 != "#") { print "T" i "	" $0; i++}}' $file > $file.2
    mv $file.2 $file
done
