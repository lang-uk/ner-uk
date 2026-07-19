---
pretty_name: NER-UK 2.0
language:
- uk
license: cc-by-nc-sa-4.0
task_categories:
- token-classification
task_ids:
- named-entity-recognition
annotations_creators:
- expert-generated
language_creators:
- found
multilinguality:
- monolingual
size_categories:
- n<1K
source_datasets:
- original
tags:
- ner
- ukrainian
- brat
configs:
- config_name: default
  data_files:
  - split: train
    path: data/train.parquet
  - split: test
    path: data/test.parquet
---

# NER-UK 2.0: A Rich Corpus of Named Entities for Ukrainian

NER-UK 2.0 is a corpus of 560 Ukrainian texts manually annotated for the
named entity recognition task, developed by the [lang-uk](https://lang-uk.github.io/)
community. It contains 21,993 entities of 13 types spanning two genres:
fiction and non-fiction texts from the [Brown-UK corpus](https://github.com/brown-uk/corpus)
(`bruk`) and investigative journalism articles from
[«Наші гроші»](https://nashigroshi.org) (`ng`).

This dataset is a token-level IOB2 export of the corpus, generated
deterministically from the original Brat standoff annotations in the
[lang-uk/ner-uk](https://github.com/lang-uk/ner-uk) repository by
[`scripts/export_huggingface.py`](https://github.com/lang-uk/ner-uk/blob/master/scripts/export_huggingface.py).
Nested entities were removed (keeping the outermost entity), which leaves
21,546 entities in this export. The train/test split is the official
`v2.0/data/dev-test-split.txt` split from the repository.

## Dataset structure

Each record is one document:

| Field         | Type           | Description                                        |
|---------------|----------------|----------------------------------------------------|
| `document_id` | `string`       | File name of the source document (without extension) |
| `source`      | `string`       | Sub-corpus: `bruk` or `ng`                         |
| `tokens`      | `list[string]` | Tokens of the document                             |
| `ner_tags`    | `list[string]` | IOB2 tag per token (e.g. `B-PERS`, `I-PERS`, `O`)  |

Documents are provided whole (not split into sentences) to preserve full
context; blank-line paragraph boundaries from the source corpus are not kept.
The same data is also available as human-diffable JSON Lines in the
[`jsonl/`](https://github.com/lang-uk/ner-uk/tree/master/huggingface/jsonl) folder.

### Splits

| Split | Documents | Tokens  | Entities |
|-------|-----------|---------|----------|
| train | 391       | 217,887 | 14,735   |
| test  | 169       | 103,737 | 6,811    |

The `train` split corresponds to the `DEV` section of the official split file,
`test` to the `TEST` section. Please report results on the official `test` split.

### Entity types

`ART` (artifact), `DATE`, `DOC` (document), `JOB` (job title), `LOC` (location),
`MISC`, `MON` (money), `ORG` (organization), `PCT` (percentage),
`PERIOD`, `PERS` (person name), `QUANT` (quantity), `TIME`.

Entity counts in this export (after removal of nested entities):

| Type   | train | test | Total |
|--------|-------|------|-------|
| ART    | 398   | 237  | 635   |
| DATE   | 1,206 | 501  | 1,707 |
| DOC    | 101   | 40   | 141   |
| JOB    | 1,320 | 657  | 1,977 |
| LOC    | 2,161 | 818  | 2,979 |
| MISC   | 371   | 142  | 513   |
| MON    | 618   | 325  | 943   |
| ORG    | 3,642 | 1,539| 5,181 |
| PCT    | 171   | 90   | 261   |
| PERIOD | 402   | 185  | 587   |
| PERS   | 4,026 | 2,178| 6,204 |
| QUANT  | 293   | 89   | 382   |
| TIME   | 26    | 10   | 36    |

The full IOB2 tag set is `O` plus `B-`/`I-` variants of the 13 types
(27 labels).

## Usage

```python
from datasets import load_dataset

ds = load_dataset("lang-uk/ner-uk")
print(ds["train"][0]["tokens"][:10])
print(ds["train"][0]["ner_tags"][:10])
```

## Annotation process

Texts were tokenized with the [lang-uk tokenization algorithm](https://github.com/lang-uk/ner-uk/blob/master/doc/tokenization.md)
and annotated in Brat by at least two annotators per text according to the
[annotation guidelines](https://github.com/lang-uk/ner-uk/blob/master/doc/README.md);
disagreements were resolved by a third editor. See the paper for details,
including inter-annotator agreement.

## License

[Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International](http://creativecommons.org/licenses/by-nc-sa/4.0/)
(CC BY-NC-SA 4.0), same as the source repository.

## Citation

```bibtex
@inproceedings{chaplynskyi-romanyshyn-2024-introducing,
    title = "Introducing {NER}-{UK} 2.0: A Rich Corpus of Named Entities for {U}krainian",
    author = "Chaplynskyi, Dmytro and Romanyshyn, Mariana",
    booktitle = "Proceedings of the Third Ukrainian Natural Language Processing Workshop (UNLP) @ LREC-COLING 2024",
    month = may,
    year = "2024",
    address = "Torino, Italia",
    publisher = "ELRA and ICCL",
    url = "https://aclanthology.org/2024.unlp-1.4",
}
```
