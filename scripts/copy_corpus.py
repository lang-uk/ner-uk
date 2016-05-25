#!env python

##### copy corpus to another folder without metadata

##### Usage: ./copy_corpus.py path_from path_to

import os
import sys
import re

def copy_file_without_metadata(path_from, file, path_to):
    xml_re = re.compile(r"<\/?[a-zA-Z]+>", re.IGNORECASE)
    body_re = re.compile(r"<\/?body>", re.IGNORECASE)
    with open(path_from + "\\" + file, 'r', encoding="utf-8") as in_f:
        with open(path_to + "\\" + file, 'w', encoding="utf-8") as out_f:
            body = False;
            for line in in_f:
                # if body reached clean from html
                if(body):
                    if(body_re.match(line)): break
                    line = xml_re.sub('', line)
                    out_f.write(line)
                elif(body_re.match(line)): body = True

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print("Usage: ./copy_corpus.py path_from path_to")
    else:
        for f in os.listdir(sys.argv[1]):
            copy_file_without_metadata(sys.argv[1], f, sys.argv[2])

