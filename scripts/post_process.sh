#!/bin/bash

for file in *.ann
do
    awk 'BEGIN {i = 1} {
           if ($1 != "#") {
              if (substr($1,0,1) == "T") {
                print $0
              } else {
                print "T" i "	" $0; i++ 
              }
            }
         }' "$file" > "$file.2"
    mv "$file.2" "$file"
done
