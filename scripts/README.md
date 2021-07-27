# NER-UK Scripts

## Training NER models

### MITIE
Requirements:
* trained feature extractor - binary can be obtained by running `MITIE/tools/wordrep` on data set. If not provided, script will try to download pretrained model.
* For now some utilities reside in another repo which you can install via `pip3 install git+https://github.com/gawy/stanza-lang-uk.git`

Run from project root
`python3 scripts/train_mitie_ner.py`

Use `python3 scripts/ner_train_mitie.py -h` to print cmd line arguments.

The resulting model will reside in `workspace/mitie/mitie_ner_model.dat`

## Stanza
Stanza already have almost everything that is required to train on our data set merged in.
Some fixes are pending to be merged from `https://github.com/gawy/stanza.git --branch ner-languk-def-split` (ability to read train-test split from our file insted of random split).

Run: `scripts/train_stanza_ner.sh`

To use custom word vectors path it with argument `--wordvec_file`. **Stanza uses vectors of dimention `100`**.
Vector file first must be converted to .pt format (pytorch binary format) using script from stanza `convert_pretrain.py`. More details please read [here in Stanza docs](https://stanfordnlp.github.io/stanza/word_vectors.html).
Vector file to be converted - must start with line of format: `<word_count> <dimention [100]>`

Word count can usually be obtained by `wc -l <file>`

Trained model will be available under workspace/stanza folder.

## tokenize-uk.lisp

### Installation

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
