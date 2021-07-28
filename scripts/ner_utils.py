
from typing import Tuple
from collections import namedtuple
import os
from bsf_beios.bsf_to_beios import convert_bsf
from tqdm import tqdm


Markup = namedtuple('Markup', 'tag range_start range_end text')
    

def read_train_test_split(config_path):
    # Read dev/test split from config
    dev_files, test_files = [], []
    container = dev_files
    with open(config_path, 'r') as f:
        for ln in f:
            ln = ln.strip()
            if ln == 'DEV':
                container = dev_files
            elif ln == 'TEST':
                container = test_files
            elif ln == '':
                pass
            else:
                container.append(ln)
    return dev_files,test_files


def read_bsf_data(f_name):
    """
    TODO: full path or relative path will be enough
    Locate .ann and .txt for `f_name` and read data from those 2 files.
    It is assumed that files are located in `data/` directory relative to current working dir.
    :param f_name: path to file without an extension. 
    :return : tuple of (string, string) with raw data from .txt and .ann files.
    """
    path = os.path.join('data', f_name)
    # read ann
    with open (path + '.ann', 'r') as f:
        annotations = f.read()
    # read tokens
    with open (path + '.txt', 'r') as f:
        tok_txt = f.read()
    
    
    return (tok_txt, annotations)


def read_data_to_iob(file_names: list[str]):
    """
    Given the list of files in `file_names` read tokens and annotations and convert annotations to iob format.
    :param file_names: list of the names of data files to read from. (ommit the final extension part)
    """
    # read test data to iob format labels
    Y = []
    X = []
    for f_name in tqdm(file_names, total=len(file_names)):
    #     print(f_name)
        txt, ann = read_bsf_data(f_name)
        iob_lst = convert_bsf(txt, ann, 'iob').split('\n')
    #     print(iob)
        iob = [tok.split()[1] for tok in iob_lst if len(tok.split()) == 2]
    #     print(iob)
        Y.append(iob)
        X.append(txt)
    
    return (X, Y)

    