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


    url_re1 = re.compile(r"([a-zA-Z]+) . ([a-zA-Z]+)", re.IGNORECASE)
    url_re2 = re.compile(r"([a-zA-Z]+) . ([a-zA-Z]+) . ([a-zA-Z]+)", re.IGNORECASE)
    open_re = re.compile(r"([\(«“]) ", re.IGNORECASE)
    close_re = re.compile(r" ([:\)».,?”])", re.IGNORECASE)
    defis_re = re.compile(r" ([\u2013\-\+]) ", re.IGNORECASE)
    defis2_re = re.compile(r" ([\u2013\-\+])", re.IGNORECASE)
    double_space_re = re.compile(r"  ", re.IGNORECASE)
    ck_re = re.compile(r"П \(б\)\s?(У)?", re.IGNORECASE)
    names2_re = re.compile(r"([А-Я])\. ([А-Я])\. ([А-Я])")
    names_re = re.compile(r"([А-Я])\. ([А-Я])")
    for ann in annotations:
        # first need to clean tokenized annotation from spaces
        text = url_re1.sub("\g<1>.\g<2>", ann["row"][2])
        text = url_re2.sub("\g<1>.\g<2>.\g<3>", text)
        text = double_space_re.sub(" ", text)
        text = open_re.sub("\g<1>", text)
        text = close_re.sub("\g<1>", text)
        text = defis_re.sub("\g<1>", text)
        text = defis2_re.sub("\g<1>", text)
        text = ck_re.sub("П(б)\g<1>", text)
        text = names_re.sub("\g<1>.\g<2>", text)
        text = names2_re.sub("\g<1>.\g<2>.\g<3>", text)
        text = double_space_re.sub(" ", text)
        text = text.strip()
        # then go back from tokenized start position
        length = len(text)
        for i in itertools.count():
            newStart = ann["start"] - i
            if(newStart < 0):
                #raise NameError("Token {0} not found in file {1}!".format(text, filename))
                print("Token {0} not found in file {1}!".format(text, filename))
                break
            newEnd = newStart + length
            if(content[newStart:newEnd].strip() == text):
                ann["start"] = newStart
                ann["end"] = newEnd
                ann["text"] = text
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



