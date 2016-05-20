#!/bin/bash

for filename in *.txt
do
    file=$(basename "$filename" .txt)
    for dir1 in */
    do
        dir1=$(basename $dir1)
        for dir2 in */
        do
            dir2=$(basename $dir2)
            if [[ "$dir1" < "$dir2" ]] && [ -e "$dir1/$file.ann" ] && [ -e "$dir2/$file.ann" ]
            then
                diff --new-line-format='+%L' --old-line-format='-%L' --unchanged-line-format='%L' <(cut -f 2- "$dir1/$file.ann") <(cut -f 2- "$dir2/$file.ann") > "$file.ann"
                result=$?
                echo $(date -u +"%Y-%m-%dT%H:%M:%SZ") $dir1 $dir2 "$file.ann" $result >> check_log.txt                
                if [ $result -eq 0 ]
                then
                    awk 'BEGIN {i = 1} {if ($1 != "#") { print "T" i "	" $0; i++}}' "$file.ann" > "$file.ann.2"
                    mv "$file.ann.2" "$file.ann"
                elif [ $result -eq 1 ]
                then
                    echo $dir1 $dir2 | mail -a "$file.ann" -s "Conflict in $file" vseloved@gmail.com
                fi
            fi
         done
    done
done
