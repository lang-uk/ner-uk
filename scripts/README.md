# NER-UK Scripts

## Training NER models

Pre-requisites
* `pip3 install -r scripts/requirements.txt`

## To realign a dataset after proof-reading:
*This is only needed if you are extending the corpus*
```bash

python align_after_seva.py ../v2.0/source/bruk ../v2.0/output/bruk
python align_after_seva.py ../v2.0/source/ng ../v2.0/output/ng
```

## To calculate stats on dev-test split:
```bash

python calculate_split_stats.py ../v2.0/data ../v2.0/data/dev-test-split.txt
```

## Converting data to IOB/BEIOS formats
If you need data in IOB/BEIOS formats for some other kinds of training just use `convert_data.py` script.

To export in IOB format with no inner nested tags:
```bash

python  convert_data.py --src_dataset ../v2.0/data --dst /tmp/ner-output --overlap_strategy remove_inner --split_file ../v2.0/data/dev-test-split.txt
```
You can change the `overlap_strategy` parameter to remove outer overlapping tags


`python3 scripts/convert_data.py` will generate iob files according to train/test split provided.
For more parameters run `python3 scripts/convert_data.py -h`.

## Running tests
```shell
python3 -m unittest discover -s test
```

## Other scripts and recipes used in the v1.0 of the corpus

### Training your models
#### MITIE
Requirements:
* trained feature extractor - binary can be obtained by running `MITIE/tools/wordrep` on data set. If not provided, the script will try to download pre-trained model.

Run from project root
`python3 scripts/train_mitie_ner.py`

Use `python3 scripts/ner_train_mitie.py -h` to print cmd line arguments.

The resulting model will reside in `workspace/mitie/mitie_ner_model.dat`

#### Stanza
Stanza already has almost everything that is required to train on our data set. But if the dataset was updated - you can execute the training process with `scripts/`train_stanza_ner.sh`

To use custom word vectors, specify its file path with the argument `--`word_vec` or `-w`. **Stanza uses vectors of dimension `100`**. 
`scripts/train_stanza_ner.sh -w=path_to_your_wordvec_file`

Vector file must be converted to .pt format (pytorch binary format) using script from stanza `convert_pretrain.py`. For more details please read [here in Stanza docs](https://stanfordnlp.github.io/stanza/word_vectors.html).
Vector file to be converted - must start with line of format: `<word_count> <dimension [100]>`

Word count can usually be obtained by `wc -l <file>`
The trained model will be available under workspace/stanza folder.

Stanza additionally provides a pretrained Character level model. It was shown to improve the performance of the NER model by around 1%.

## Evaluating NER models
Execute: `python3 scripts/eval_ner_models.py --stanza=<path_to_stanza_model> --mitie=<path_to_mitie_model>`

The script will produce a report with the help of sklearn classification report and will print it out to the console.

## Calculating the stats on the current corpus
If you want to get the most recent numbers on the corpus (document count, token count, split between train and test sets) - just run `scripts/corpus_stats.py`


### Installation for lisp scripts used for tokenization (discontinued)

```
$ sbcl --non-interactive --eval '(compile-file "tokenize-uk--all-systems.lisp")'
```

### Usage

- tokenize text from STDIN line-by-line:

    ```
    $ sbcl --script tokenize-uk--all-systems.fasl
    ```

- tokenize files (for each file create a corresponding .tok file):

    ```
    $ sbcl --script tokenize-uk--all-systems.fasl -- file-to-tokenize1 file-to-tokenize2 ...
    ```
