#!/usr/bin/env python

# Data conversion utility for ner-uk data set. Originally created for purposes of training Stanza model.
# Later Stanza has merged some of this code inside.
# Terminology:
# IOB https://en.wikipedia.org/wiki/Inside%E2%80%93outside%E2%80%93beginning_(tagging) Some sources refer to this as BIO
# BEIOS - extended IOB with E == end (for multi word tokens)


import argparse
import logging
import os
import glob
import pathlib
from typing import Tuple

from tqdm import tqdm
from random import choices, shuffle
from os.path import splitext

from ner_utils import convert_bsf

log = logging.getLogger(__name__)
log.setLevel(logging.INFO)


def convert_bsf_in_folder(
    src_dir_path: pathlib.Path,
    dst_dir_path: pathlib.Path,
    converter: str = "beios",
    doc_delim: str = "\n",
    train_test_split_file: pathlib.Path = None,
) -> None:
    """

    :param doc_delim: delimiter to be used between documents
    :param src_dir_path: path to directory with BSF marked files
    :param dst_dir_path: where to save output data
    :param converter: `beios` or `iob` output formats
    :param train_test_split_file: path to file containing train/test lists of file names
    :return:
    """
    # following 2 constants need to comply with stanza naming for corpus and language
    corpus_name = "Ukrainian-languk"

    ann_path = src_dir_path / "*.tok.ann"
    ann_files = glob.glob(str(ann_path))
    ann_files.sort()

    tok_path = src_dir_path / "*.tok.txt"
    tok_files = glob.glob(str(tok_path))
    tok_files.sort()

    corpus_folder = dst_dir_path / corpus_name
    if not corpus_folder.exists():
        corpus_folder.mkdir(parents=True)

    if not ann_files or not tok_files:
        log.error(
            f"Token and annotation files are not found at specified path {ann_path}"
        )
        return
    if len(ann_files) != len(tok_files):
        log.error(
            f"Mismatch between Annotation and Token files. Ann files: {len(ann_files)}, token files: {len(tok_files)}"
        )
        return

    train_set = []
    dev_set = []
    test_set = []

    data_sets = [train_set, dev_set, test_set]
    split_weights = (8, 1, 1)

    if train_test_split_file is not None:
        train_names, dev_names, test_names = read_languk_train_test_split(
            train_test_split_file, 0
        )

    log.info(f'Found {len(tok_files)} files in data folder "{src_dir_path}"')
    for tok_fname, ann_fname in tqdm(
        zip(tok_files, ann_files), total=len(tok_files), unit="file"
    ):
        if splitext(tok_fname)[0] != splitext(ann_fname)[0]:
            tqdm.write(
                f"Token and Annotation file names do not match ann={ann_fname}, tok={tok_fname}"
            )
            continue

        with open(tok_fname) as tok_file, open(ann_fname) as ann_file:
            token_data = tok_file.read()
            ann_data = ann_file.read()
            out_data = convert_bsf(token_data, ann_data, converter)

            if train_test_split_file is None:
                target_dataset = choices(data_sets, split_weights)[0]
            else:
                target_dataset = train_set
                fkey = splitext(os.path.basename(tok_fname))[0]
                if fkey in dev_names:
                    target_dataset = dev_set
                elif fkey in test_names:
                    target_dataset = test_set

            target_dataset.append(out_data)
    log.info(
        f"Data is split as following: train={len(train_set)}, dev={len(dev_set)}, test={len(test_set)}"
    )

    # writing data to {train/dev/test}.iob files
    names = ["train", "dev", "test"]
    if doc_delim != "\n":
        doc_delim = "\n" + doc_delim + "\n"
    for idx, name in enumerate(names):
        fname = corpus_folder / (name + ".iob")
        with fname.open("w") as f:
            f.write(doc_delim.join(data_sets[idx]))
        log.info("Writing to " + str(fname))

    log.info("All done")


def read_languk_train_test_split(
    file_path: pathlib.Path, dev_split: float = 0.1
) -> Tuple:
    """
    Read predefined split of train and test files in data set.
    Originally located under doc/dev-test-split.txt
    :param file_path: path to dev-test-split.txt file (should include file name with extension)
    :param dev_split: 0 to 1 float value defining how much to allocate to dev split
    :return: tuple of (train, dev, test) each containing list of files to be used for respective data sets
    """
    log.info(
        f'Trying to read train/dev/test split from file "{str(file_path)}". Dev allocation = {dev_split}'
    )
    train_files, test_files, dev_files = [], [], []
    container = test_files
    with file_path.open("r") as f:
        for ln in f:
            ln = ln.strip()
            if ln == "DEV":
                container = train_files
            elif ln == "TEST":
                container = test_files
            elif ln == "":
                pass
            else:
                container.append(ln)

    # split in file only contains train and test split.
    # For Stanza training we need train, dev, test
    # We will take part of train as dev set
    # This way anyone using test set outside of this code base can be sure that there was no data set pollution
    shuffle(train_files)
    dev_files = train_files[: int(len(train_files) * dev_split)]
    train_files = train_files[int(len(train_files) * dev_split) :]

    assert len(set(train_files).intersection(set(dev_files))) == 0

    log.info(
        f"Files in each set: train={len(train_files)}, dev={len(dev_files)}, test={len(test_files)}"
    )
    return train_files, dev_files, test_files


if __name__ == "__main__":
    logging.basicConfig()

    parser = argparse.ArgumentParser(
        description="Convert lang-uk NER data set from Brat stadoff format to BEIOS or IOB format"
        " (compatible with Stanza NER model training requirements)."
    )
    parser.add_argument(
        "-c",
        type=str,
        default="iob",
        help="`beios` or `iob` formats to be used for output",
    )

    parser.add_argument(
        "--src_dataset",
        type=pathlib.Path,
        default="data",
        help='Dir with lang-uk dataset "data" folder (https://github.com/lang-uk/ner-uk/data)',
    )
    parser.add_argument(
        "--dst",
        type=pathlib.Path,
        default="workspace/data",
        help="Where to store the converted dataset",
    )

    parser.add_argument(
        "--doc_delim",
        type=str,
        default="\n",
        help="Delimiter to be used to separate documents in the output data",
    )

    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "--split_file",
        type=pathlib.Path,
        default="doc/dev-test-split.txt",
        help="Name of a file containing Train/Test split (files in train and test set)",
    )
    group.add_argument(
        "--split_randomly",
        action="store_true",
        help="Randomly create train/dev/test sets from src_dataset.",
    )

    parser.print_usage()
    args = parser.parse_args()

    split_file = None if args.split_randomly else args.split_file
    convert_bsf_in_folder(
        args.src_dataset,
        args.dst,
        args.c,
        args.doc_delim,
        train_test_split_file=split_file,
    )
