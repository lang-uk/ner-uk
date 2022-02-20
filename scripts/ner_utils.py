from collections import namedtuple
import os
from tqdm import tqdm
import re

BsfInfo = namedtuple('BsfInfo', 'id, tag, start_idx, end_idx, token')
    

def format_token_as_beios(token: str, tag: str) -> list:
    t_words = token.split()
    res = []
    if len(t_words) == 1:
        res.append(token + ' S-' + tag)
    else:
        res.append(t_words[0] + ' B-' + tag)
        for t_word in t_words[1: -1]:
            res.append(t_word + ' I-' + tag)
        res.append(t_words[-1] + ' E-' + tag)
    return res


def format_token_as_iob(token: str, tag: str) -> list:
    t_words = token.split()
    res = []
    if len(t_words) == 1:
        res.append(token + ' B-' + tag)
    else:
        res.append(t_words[0] + ' B-' + tag)
        for t_word in t_words[1:]:
            res.append(t_word + ' I-' + tag)
    return res


def convert_bsf(data: str, bsf_markup: str, converter: str = 'beios') -> str:
    """
    Convert data file with NER markup in Brat standoff Format to BEIOS or IOB format.

    :param converter: iob or beios converter to use for document
    :param data: tokenized data to be converted. Each token separated with a space
    :param bsf_markup: Brat standoff Format markup
    :return: data in BEIOS or IOB format https://en.wikipedia.org/wiki/Inside–outside–beginning_(tagging)
    """

    def join_simple_chunk(chunk: str) -> list:
        if len(chunk.strip()) == 0:
            return []
        # keep the newlines, but discard the non-newline whitespace
        tokens = re.split(r'(\n)|\s', chunk.strip())
        # the re will return None for splits which were not caught in a group
        tokens = [x for x in tokens if x is not None]
        return [token + ' O' if len(token.strip()) > 0 else token for token in tokens]

    converters = {'beios': format_token_as_beios, 'iob': format_token_as_iob}
    res = []
    markup = parse_bsf(bsf_markup)

    prev_idx = 0
    m_ln: BsfInfo
    convert_f = converters[converter]
    for m_ln in markup:
        res += join_simple_chunk(data[prev_idx:m_ln.start_idx])

        res.extend(convert_f(m_ln.token, m_ln.tag))
        prev_idx = m_ln.end_idx

    if prev_idx < len(data) - 1:
        res += join_simple_chunk(data[prev_idx:])

    return '\n'.join(res)


def parse_bsf(bsf_data: str) -> list:
    """
    Convert multiline textual bsf representation to a list of named entities.

    :param bsf_data: data in the format 'T9	PERS 778 783    токен'. Can be multiple lines.
    :return: list of named tuples for each line of the data representing a single named entity token
    """
    if len(bsf_data.strip()) == 0:
        return []

    ln_ptrn = re.compile(r'(T\d+)\s(\w+)\s(\d+)\s(\d+)\s(.+?)(?=T\d+\s\w+\s\d+\s\d+|$)', flags=re.DOTALL)
    result = []
    for m in ln_ptrn.finditer(bsf_data.strip()):
        bsf = BsfInfo(m.group(1), m.group(2), int(m.group(3)), int(m.group(4)), m.group(5).strip())
        result.append(bsf)
    return result

def read_train_test_split(config_path):
    """
    :param config_path: path to file with dev, test split. File containing file names.
    :return: tuple of (dev, test) where dev and test are lists of file names from config_path file
    """
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

    return tok_txt, annotations


def read_data_to_iob(file_names: list[str]):
    """
    Given the list of files in `file_names` read tokens and annotations and convert annotations to iob format.
    :param file_names: list of the names of data files to read from. (ommit the final extension part)
    """
    # read test data to iob format labels
    Y = []
    X = []
    for f_name in tqdm(file_names, total=len(file_names)):
        txt, ann = read_bsf_data(f_name)
        iob_lst = convert_bsf(txt, ann, 'iob').split('\n')
    #     print(iob)
        iob = [tok.split()[1] for tok in iob_lst if len(tok.split()) == 2]
    #     print(iob)
        Y.append(iob)
        X.append(txt)
    
    return X, Y

    