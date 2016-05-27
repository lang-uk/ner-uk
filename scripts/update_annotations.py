#!env python

##### updates annotation to match raw (not tokenized) positions

##### Usage: ./update_annotations.py path_to_data

import os
import sys
import itertools
import re

def update_annotations_in_file(path, filename):
    # first read annotations from tokenized file
    annotations = []
    ann_path = path + "\\" + filename + ".tok.ann"
    if(not os.path.isfile(ann_path)): return

    with open(ann_path, "r", encoding="utf-8") as ann_f:
        for ann_l in ann_f:
            ann_row = ann_l.split("\t")
            annotation = {}
            annotation["row"] = ann_row
            annotation["text"] = ann_row[2].strip("\n")
            start_end = ann_row[1].split(' ')
            annotation["type"] = start_end[0]
            annotation["start"] = int(start_end[1])
            annotation["end"] = int(start_end[2])
            annotations.append(annotation)
    # assume that tokenized file has structure where position of words always >= from position in original file
    # because of additional spcaces and linebreaks
    content = ""
    with open(path + "\\" + filename + ".txt", "r", encoding="utf-8")  as txt_f:
        content = txt_f.read()

    for ann in annotations:
        missed_re = re.compile(r"[\[\]‐\s\-№]", re.IGNORECASE)
        # first clean tokenized annotation from spaces
        text = missed_re.sub("", ann["row"][2])
        # then go back from tokenized start position
        length = len(ann["row"][2]) + 3 # to be sure
        missed_simbols = " []-‐№"
        for i in itertools.count():
            newStart = ann["start"] - i
            if(newStart < 0):
                print("Token {0} not found in file {1}!".format(ann["row"][2].strip(), filename))
                break
            newEnd = newStart + length

            # clean original text from spaces
            original_text_escaped = missed_re.sub("", content[newStart:newEnd])
            position = original_text_escaped.find(text)
            if(position == 0):
                # now fix position by restoring spaces
                original_text = content[newStart:newEnd]
                newEnd = newStart + len(text)
                for i in range(len(original_text)):
                    if(original_text[i] in missed_simbols):
                        if(i <= newEnd-newStart-1):
                            newEnd = newEnd + 1
                        else: break
                ann["start"] = newStart
                ann["end"] = newEnd
                ann["text"] = content[newStart:newEnd].strip()
                break

    # and save result
    with open(path + "\\" + filename + ".ann", "w", encoding="utf-8")  as ann_f:
        for ann in annotations:
            if("text" in ann):
                ann_f.write("{0}\t{1} {2} {3}\t{4}\n".format(ann["row"][0], ann["type"], ann["start"], ann["end"], ann["text"]))

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("./update_annotations.py path_to_data")
    else:
        for f in os.listdir(sys.argv[1]):
            filename, file_extension = os.path.splitext(f)
            if(file_extension == ".txt"):
                second_name, second_extension = os.path.splitext(filename)
                if(len(second_extension) != 3):
                    update_annotations_in_file(sys.argv[1],filename)



