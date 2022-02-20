#!/usr/bin/env python

# Calculate basic statistics of the data set

from pathlib import Path
print(Path().resolve())

from convert_data import read_languk_train_test_split
from ner_utils import parse_bsf

from collections import Counter
from itertools import chain


def read_data(root_path:Path, f_names: list):
    """
    Read data from a list of files in `f_names`.
    :param root_path: path of the working dir
    :param f_names: list of file names to read from
    :return: list of tuples (document split in sentences, annotation data in BsfInfo structures)
    """
    base_path = root_path / Path('data/')
    
    tok_storage = []
    ann_storage = []
    for f_name in f_names:
        # read ann
        with open(base_path / (f_name + '.ann'), 'r') as f:
            annotations = parse_bsf(f.read())
        # read tokens
        with open(base_path / (f_name + '.txt'), 'r') as f:
            tok_txt = f.read()

        tok_sent = [sent for sent in tok_txt.split('\n') if len(sent) > 0]
        tok_storage.append(tok_sent)
        ann_storage.append(annotations)
    
    return tok_storage, ann_storage


# check where we are located and resolve path properly
# to safely execute from within or outside of the scripts folder
root_path: str
if Path('./data').exists():
    root_path = Path('./')
elif Path('../data').exists():
    root_path = Path('../')

train_names, dev_names, test_names = read_languk_train_test_split(root_path / Path("doc/dev-test-split.txt"), 0)

#doc sent
train_cnt = len(train_names)
test_cnt = len(test_names)
total_cnt = train_cnt + test_cnt


train_data, train_ann = read_data(root_path, train_names)
test_data, test_ann = read_data(root_path, test_names)

# sentense stats
train_sent_cnt = [len(sents) for sents in train_data]

test_sent_cnt = [len(sents) for sents in test_data]

train_sent_n = sum(train_sent_cnt)
test_sent_n = sum(test_sent_cnt)
total_sent = train_sent_n + test_sent_n

# Tokens
train_tok_cnt = len(list(chain.from_iterable(sent.split() for sent in chain.from_iterable(train_data))))
test_tok_cnt = len(list(chain.from_iterable(sent.split() for sent in chain.from_iterable(test_data))))

# Tags

ann_train_cnt = Counter([ann.tag for ann in chain.from_iterable(train_ann)])
ann_test_ctn = Counter([ann.tag for ann in chain.from_iterable(test_ann)])

# tok_train_cnt = Counter([ for sent in chain.from_iterable(train_data)])
ann_lbl = list(ann_train_cnt)


# Print it all 
print(f'Documents: train={train_cnt}, test={test_cnt}, split={train_cnt/total_cnt :.2f}/{test_cnt/total_cnt :.2f}')

print(f'Sentence stats: train={train_sent_n}, test={test_sent_n}, split={train_sent_n/total_sent :.2f}/{test_sent_n/total_sent :.2f}')

print('Train set tags')
print(ann_train_cnt)
print(*[f'{lbl}: {(ann_train_cnt[lbl]/sum(ann_train_cnt.values())) :.2f} ' for lbl in ann_lbl])


print('\nTest set tags')
print(ann_test_ctn)
print(*[f'{lbl}: {(ann_test_ctn[lbl]/sum(ann_test_ctn.values())) :.2f} ' for lbl in ann_lbl])


total_tag_cnt = {lbl: ann_test_ctn[lbl] + ann_train_cnt[lbl] for lbl in ann_lbl}

print(f'\nSplit train/test:')

print(*[f'{lbl}={ann_train_cnt[lbl]/total_tag_cnt[lbl] :.2f}/{ann_test_ctn[lbl]/total_tag_cnt[lbl] :.2f}' for lbl in ann_lbl])


print('\nAbsolute total:')
print(f'- {train_cnt + test_cnt} текстів')
print(f'- {train_tok_cnt + test_tok_cnt :_} токенів')
print(f'- {sum(total_tag_cnt.values()) :_} сутностей NER')
print(f'  - ПЕРС {ann_train_cnt["PERS"] + ann_test_ctn["PERS"]:_}')
print(f'  - ЛОК {ann_train_cnt["LOC"] + ann_test_ctn["LOC"]:_}')
print(f'  - ОРГ {ann_train_cnt["ORG"] + ann_test_ctn["ORG"]:_}')
print(f'  - РІЗН {ann_train_cnt["MISC"] + ann_test_ctn["MISC"]:_}')

