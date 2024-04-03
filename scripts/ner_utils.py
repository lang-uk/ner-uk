import os
import re
from typing import List
import pathlib
from enum import StrEnum
from collections import namedtuple
from typing import Generator

from tqdm import tqdm


BsfInfo = namedtuple("BsfInfo", "id, tag, start_idx, end_idx, token, comment", defaults=[None])
class OverlapStrategy(StrEnum):
    """
    Strategy to be used for handling overlapping entities
    """

    REMOVE_INNER = "remove_inner"
    REMOVE_OUTER = "remove_outer"


def bsf_to_str(bsf: BsfInfo) -> str:
    return f"T{bsf.id}\t{bsf.tag}\t{bsf.start_idx}\t{bsf.end_idx}\t{bsf.token}"


def format_token_as_beios(token: str, tag: str) -> list:
    t_words = token.split()
    res = []
    if len(t_words) == 1:
        res.append(token + " S-" + tag)
    else:
        res.append(t_words[0] + " B-" + tag)
        for t_word in t_words[1:-1]:
            res.append(t_word + " I-" + tag)
        res.append(t_words[-1] + " E-" + tag)
    return res


def format_token_as_iob(token: str, tag: str) -> list:
    t_words = token.split()
    res = []
    if len(t_words) == 1:
        res.append(token + " B-" + tag)
    else:
        res.append(t_words[0] + " B-" + tag)
        for t_word in t_words[1:]:
            res.append(t_word + " I-" + tag)
    return res


def filter_overlap(
    bsfs: List[BsfInfo], overlap_strategy: OverlapStrategy
) -> List[BsfInfo]:
    """
    Calculates number of full overlaps and partial overlaps in the list of BsfInfo
    Args:
        bsfs: list of BsfInfo
        overlap_strategy: how to handle overlapping entities
    Returns:
        List of filtered BsfInfos

    Examples:
    >>> filter_overlap([BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=1, token='a', comment=None), BsfInfo(id=2, tag='tag2', start_idx=1, end_idx=2, token='b', comment=None)], OverlapStrategy.REMOVE_INNER)
    [BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=1, token='a', comment=None), BsfInfo(id=2, tag='tag2', start_idx=1, end_idx=2, token='b', comment=None)]
    >>> filter_overlap([BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=1, token='a', comment=None), BsfInfo(id=2, tag='tag2', start_idx=1, end_idx=2, token='b', comment=None)], OverlapStrategy.REMOVE_OUTER)
    [BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=1, token='a', comment=None), BsfInfo(id=2, tag='tag2', start_idx=1, end_idx=2, token='b', comment=None)]

    >>> filter_overlap([BsfInfo(id=2, tag='tag2', start_idx=1, end_idx=2, token='b', comment=None), BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=1, token='a', comment=None)], OverlapStrategy.REMOVE_INNER)
    [BsfInfo(id=2, tag='tag2', start_idx=1, end_idx=2, token='b', comment=None), BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=1, token='a', comment=None)]
    >>> filter_overlap([BsfInfo(id=2, tag='tag2', start_idx=1, end_idx=2, token='b', comment=None), BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=1, token='a', comment=None)], OverlapStrategy.REMOVE_OUTER)
    [BsfInfo(id=2, tag='tag2', start_idx=1, end_idx=2, token='b', comment=None), BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=1, token='a', comment=None)]

    >>> filter_overlap([BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=2, token='ab', comment=None), BsfInfo(id=2, token="b", start_idx=1, end_idx=2, tag="tag2")], overlap_strategy=OverlapStrategy.REMOVE_INNER)
    [BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=2, token='ab', comment=None)]
    >>> filter_overlap([BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=2, token='ab', comment=None), BsfInfo(id=2, tag='tag2', start_idx=1, end_idx=2, token='b', comment=None)], overlap_strategy=OverlapStrategy.REMOVE_OUTER)
    [BsfInfo(id=2, tag='tag2', start_idx=1, end_idx=2, token='b', comment=None)]

    >>> filter_overlap([BsfInfo(id=2, token="b", start_idx=1, end_idx=2, tag="tag2"), BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=2, token='ab', comment=None)], overlap_strategy=OverlapStrategy.REMOVE_INNER)
    [BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=2, token='ab', comment=None)]
    >>> filter_overlap([BsfInfo(id=2, tag='tag2', start_idx=1, end_idx=2, token='b', comment=None), BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=2, token='ab', comment=None)], overlap_strategy=OverlapStrategy.REMOVE_OUTER)
    [BsfInfo(id=2, tag='tag2', start_idx=1, end_idx=2, token='b', comment=None)]

    >>> filter_overlap([BsfInfo(id=2, token="a", start_idx=0, end_idx=1, tag="tag2"), BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=2, token='ab', comment=None)], overlap_strategy=OverlapStrategy.REMOVE_INNER)
    [BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=2, token='ab', comment=None)]
    >>> filter_overlap([BsfInfo(id=2, tag='tag2', start_idx=0, end_idx=1, token='a', comment=None), BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=2, token='ab', comment=None)], overlap_strategy=OverlapStrategy.REMOVE_OUTER)
    [BsfInfo(id=2, tag='tag2', start_idx=0, end_idx=1, token='a', comment=None)]

    >>> filter_overlap([BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=3, token='abc', comment=None), BsfInfo(id=2, token="b", start_idx=1, end_idx=2, tag="tag2")], overlap_strategy=OverlapStrategy.REMOVE_INNER)
    [BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=3, token='abc', comment=None)]
    >>> filter_overlap([BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=3, token='abc', comment=None), BsfInfo(id=2, tag='tag2', start_idx=1, end_idx=2, token='b', comment=None)], overlap_strategy=OverlapStrategy.REMOVE_OUTER)
    [BsfInfo(id=2, tag='tag2', start_idx=1, end_idx=2, token='b', comment=None)]

    >>> filter_overlap([BsfInfo(id=2, token="b", start_idx=1, end_idx=2, tag="tag2"), BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=3, token='abc', comment=None)], overlap_strategy=OverlapStrategy.REMOVE_INNER)
    [BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=3, token='abc', comment=None)]
    >>> filter_overlap([BsfInfo(id=2, tag='tag2', start_idx=1, end_idx=2, token='b', comment=None), BsfInfo(id=1, tag='tag1', start_idx=0, end_idx=3, token='abc', comment=None)], overlap_strategy=OverlapStrategy.REMOVE_OUTER)
    [BsfInfo(id=2, tag='tag2', start_idx=1, end_idx=2, token='b', comment=None)]

    >>> filter_overlap([BsfInfo(id=1, token="ab", start_idx=0, end_idx=2, tag="tag1"), BsfInfo(id=2, token="bc", start_idx=1, end_idx=3, tag="tag2")], overlap_strategy=OverlapStrategy.REMOVE_INNER) # doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: Partial overlap detected: {bsf1} and {bsf2}
    >>> filter_overlap([BsfInfo(id=1, token="ab", start_idx=0, end_idx=2, tag="tag1"), BsfInfo(id=2, token="bc", start_idx=1, end_idx=3, tag="tag2")], overlap_strategy=OverlapStrategy.REMOVE_OUTER) # doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: Partial overlap detected: {bsf1} and {bsf2}
    >>> filter_overlap([BsfInfo(id=2, token="bc", start_idx=1, end_idx=3, tag="tag2"), BsfInfo(id=1, token="ab", start_idx=0, end_idx=2, tag="tag1")], overlap_strategy=OverlapStrategy.REMOVE_INNER) # doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: Partial overlap detected: {bsf1} and {bsf2}
    >>> filter_overlap([BsfInfo(id=2, token="bc", start_idx=1, end_idx=3, tag="tag2"), BsfInfo(id=1, token="ab", start_idx=0, end_idx=2, tag="tag1")], overlap_strategy=OverlapStrategy.REMOVE_OUTER) # doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    ValueError: Partial overlap detected: {bsf1} and {bsf2}

    """

    results = []
    for i, bsf1 in enumerate(bsfs):
        drop = False
        for j, bsf2 in enumerate(bsfs):
            if i == j:
                continue

            start_in = (
                bsf1.start_idx >= bsf2.start_idx and bsf1.start_idx < bsf2.end_idx
            )
            end_in = bsf1.end_idx > bsf2.start_idx and bsf1.end_idx <= bsf2.end_idx

            reverse_start_in = (
                bsf2.start_idx >= bsf1.start_idx and bsf2.start_idx < bsf1.end_idx
            )
            reverse_end_in = (
                bsf2.end_idx > bsf1.start_idx and bsf2.end_idx <= bsf1.end_idx
            )

            if start_in and end_in:
                if overlap_strategy == OverlapStrategy.REMOVE_INNER:
                    drop = True
            elif (start_in or end_in) and not (reverse_start_in and reverse_end_in):
                raise ValueError(f"Partial overlap detected: {bsf1} and {bsf2}")

            if reverse_start_in and reverse_end_in:
                if overlap_strategy == OverlapStrategy.REMOVE_OUTER:
                    drop = True

        if not drop:
            results.append(bsf1)

    return results


def convert_bsf(
    data: str,
    bsf_markup: str,
    converter: str = "beios",
    overlap_strategy: OverlapStrategy = OverlapStrategy.REMOVE_INNER,
) -> str:
    """
    Convert data file with NER markup in Brat standoff Format to BEIOS or IOB format.

    :param converter: iob or beios converter to use for document
    :param data: tokenized data to be converted. Each token separated with a space
    :param bsf_markup: Brat standoff Format markup
    :param overlap_strategy: strategy to be used for handling overlapping entities
    :return: data in BEIOS or IOB format https://en.wikipedia.org/wiki/Inside–outside–beginning_(tagging)
    """

    def join_simple_chunk(chunk: str) -> list:
        if len(chunk.strip()) == 0:
            return []
        # keep the newlines, but discard the non-newline whitespace
        tokens = re.split(r"(\n)|\s", chunk.strip())
        # the re will return None for splits which were not caught in a group
        tokens = [x for x in tokens if x is not None]
        return [token + " O" if len(token.strip()) > 0 else token for token in tokens]

    converters = {"beios": format_token_as_beios, "iob": format_token_as_iob}
    res = []
    markup = parse_bsf(bsf_markup)
    markup = filter_overlap(markup, overlap_strategy)

    prev_idx = 0
    m_ln: BsfInfo
    convert_f = converters[converter]
    for m_ln in markup:
        res += join_simple_chunk(data[prev_idx : m_ln.start_idx])

        res.extend(convert_f(m_ln.token, m_ln.tag))
        prev_idx = m_ln.end_idx

    if prev_idx < len(data) - 1:
        res += join_simple_chunk(data[prev_idx:])

    return "\n".join(res)


def parse_bsf(bsf_data: str) -> list:
    """
    Convert multiline textual bsf representation to a list of named entities.

    :param bsf_data: data in the format 'T9 PERS 778 783    токен'. Can be multiple lines.
    :return: list of named tuples for each line of the data representing a single named entity token
    """
    if len(bsf_data.strip()) == 0:
        return []

    ln_ptrn = re.compile(r"(T\d+)\s(\w+)\s(\d+)\s(\d+)\s(.+?)(?=T\d+\s\w+\s\d+\s\d+|$)", flags=re.DOTALL)
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
    with open(config_path, "r") as f:
        for ln in f:
            ln = ln.strip()
            if ln == "DEV":
                container = dev_files
            elif ln == "TEST":
                container = test_files
            elif ln == "":
                pass
            else:
                container.append(ln)
    return dev_files, test_files


def read_bsf_data(f_name):
    """
    TODO: full path or relative path will be enough
    Locate .ann and .txt for `f_name` and read data from those 2 files.
    It is assumed that files are located in `data/` directory relative to current working dir.
    :param f_name: path to file without an extension.
    :return : tuple of (string, string) with raw data from .txt and .ann files.
    """
    path = os.path.join("data", f_name)
    # read ann
    with open(path + ".ann", "r") as f:
        annotations = f.read()
    # read tokens
    with open(path + ".txt", "r") as f:
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
        iob_lst = convert_bsf(txt, ann, "iob").split("\n")
        #     print(iob)
        iob = [tok.split()[1] for tok in iob_lst if len(tok.split()) == 2]
        #     print(iob)
        Y.append(iob)
        X.append(txt)

    return X, Y


def read_proofreaded_bsf_data(f_name: pathlib.Path) -> Generator[BsfInfo, None, None]:
    """
    For some reason the BSF data generated by Vsevolod for proof-reading
    has slightly different format. This function will try to fix it
    Another twist with that file format is that some tokens are multiline
    """
    bsf_regex: re.Pattern = re.compile(r"(\w+)\s(\d+)\s(\d+)\s(.+?)(?=T\d+\s\w+\s\d+\s\d+|$)", flags=re.DOTALL)
    current_token: int = 0
    tag: str = ""
    start_idx: int 
    end_idx: int
    token: str
    is_comment: bool = False
    is_token: bool = False

    current_comment = ""
    for i, line in enumerate(map(str.strip, f_name.open("r"))):
        if not line:
            continue

        if line.startswith("#"):
            if is_token:
                current_token += 1
                yield BsfInfo(current_token, tag, start_idx, end_idx, token, current_comment)
                current_comment = ""

            is_comment = True
            is_token = False

            # Skipping the comments
            current_comment = line.lstrip("#").strip()
            continue

        m = bsf_regex.search(line)
        if m:
            if is_token:
                current_token += 1
                yield BsfInfo(current_token, tag, start_idx, end_idx, token, current_comment)
                current_comment = ""

            is_comment = False
            is_token = True

            # "id, tag, start_idx, end_idx, token"
            tag = m.group(1)
            start_idx = int(m.group(2))
            end_idx = int(m.group(3))
            token = m.group(4).strip()
        else:
            if is_comment:
                current_comment += "\n" + line
                continue
            elif is_token:
                token += "\n" + line
            else:
                print(f"Cannot parse line #{i} ({line}) from the file {f_name}")

    # Leftovers
    if is_token:
        current_token += 1
        yield BsfInfo(current_token, tag, start_idx, end_idx, token, current_comment)
