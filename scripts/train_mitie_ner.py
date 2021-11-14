#!/usr/bin/env python

import shutil
import requests
from bsf_beios.bsf_to_beios import BsfInfo, parse_bsf
from mitie import *
import argparse
import os
import multiprocessing
from ner_utils import read_train_test_split

"""
Script intended to train mitie NER model using lang-uk data set in current repository.
Run `python3 scripts/train_mitie_ner.py` from root to run with default configuration.
Check down below in the file for all cmd line arguments.
"""

# don't include url params at the end - logic is trying to parse for extension of the file.
feature_extractor_url = 'https://lang.org.ua/static/downloads/ner-aux/total_word_feature_extractor.tokenized.400k.dat.zip'


def prepare_mitie_training_data(dev_files):
    # convert char offset in ner-uk markup to token based MITIE markup
    # and create MITIE samples
    base_path = './data/'
    samples = []
    for f_name in dev_files:
        # read ann
        with open(base_path + f_name + '.ann', 'r') as f:
            annotations = parse_bsf(f.read())
        # read tokens
        with open(base_path + f_name + '.txt', 'r') as f:
            tok_txt = f.read()

        tokens = tok_txt.split()

        # convert char offset to token offset
        tok_ann = []
        tok_idx = 0

        ann: BsfInfo
        for ann in annotations:
            tok_start = 0
            in_token = False
            tok_end = 0
            for i in range(tok_idx, len(tokens)):
                tok_idx = i + 1
                if not in_token and ann.token.startswith(tokens[i]):
                    tok_start = i
                    tok_end = i + 1
                    in_token = (len(ann.token) != len(tokens[i]))
                    if len(ann.token) == len(tokens[i]):
                        break
                elif in_token and ann.token.endswith(tokens[i]):
                    tok_end = i + 1
                    in_token = False
                    break
            tok_ann.append(BsfInfo(ann.id, ann.tag, tok_start, tok_end, ann.token))

        # Create MITIE sample
        sample = ner_training_instance(tokens)
        for t_ann in tok_ann:
            sample.add_entity(xrange(t_ann.start_idx, t_ann.end_idx), t_ann.tag)
        samples.append(sample)

    print(f'Converted to MITIE format. Sample documents {len(samples)}')
    return samples


def download_file(download_url, file_name):
    with requests.get(download_url, stream=True) as r:
        with open(file_name, 'wb') as f:
            shutil.copyfileobj(r.raw, f)
    return file_name


def run_training(cpu_threads, config_path, feature_extractor_path):
    dev_files, test_files = read_train_test_split(config_path)
    print(f'Loaded corpus file split configuration (documents): DEV={len(dev_files)}, TEST={len(test_files)}')

    samples = prepare_mitie_training_data(dev_files)

    # check for workspace folder existence
    workspace_folder = os.path.join('workspace', 'mitie')
    if not os.path.exists(workspace_folder):
        os.makedirs(workspace_folder)

    # Training
    if not feature_extractor_path or len(feature_extractor_path.strip()) == 0:
        # try to download pretrained file
        feature_extractor_path = os.path.join(workspace_folder, 'total_word_feature_extractor.tokenized.400k.dat')

        if not os.path.exists(feature_extractor_path):
            print(f'Feature extractor file not provided or not found. '
                  f'\nTrying to download from {feature_extractor_url}'
                  f'\nSorry no progress bar.')

            ext = feature_extractor_url.split('.')[-1]
            download_file(feature_extractor_url, feature_extractor_path + '.' + ext)
            shutil.unpack_archive(feature_extractor_path + '.' + ext, workspace_folder)
            os.remove(feature_extractor_path + '.' + ext)

    trainer = ner_trainer(feature_extractor_path)

    for s in samples:
        trainer.add(s)

    trainer.num_threads = cpu_threads

    print("Launching training process... go get a cup of tea... it's gonna be slow")
    # takes long here
    ner = trainer.train()

    model_path = os.path.join(workspace_folder, "mitie_ner_model.dat")
    ner.save_to_disk(model_path)
    print(f'Training finished. Model saved to "{model_path}"')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Run MITIE training process using annotated NER data from `data` '
                                                 'folder and using pretrained feature extractor')

    parser.add_argument('--fte_path', type=str,
                        help='Path to pretrained FeaTure Extractor. For instructions on how to train one - read '
                             'https://github.com/mit-nlp/MITIE/blob/master/examples/python/train_ner.py. '
                             'If not provided, this script will try to download some of the prior versions of it.')
    parser.add_argument('--threads', type=int, default=multiprocessing.cpu_count(),
                        help='Number of threads to use for training.')
    parser.add_argument('--split_file', type=str, default='doc/dev-test-split.txt',
                        help='Path to txt file with Train/Test split.')

    parser.print_usage()

    args = parser.parse_args()

    if not os.path.exists('data'):
        print("Error: data folder not found. Make sure you are running this script from the ner-uk project root")
    else:
        run_training(args.threads, args.split_file, args.fte_path)
