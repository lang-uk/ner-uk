# NER-UK Scripts

## Training NER models

### MITIE
Requirements:
* trained feature extractor - binary can be obtained by running `MITIE/tools/wordrep` on data set. If not provided, script will try to download pretrained model.
* For now some utilities reside in another repo which you can install via `pip3 install git+https://github.com/gawy/stanza-lang-uk.git`

Run from project root
`python3 scripts/ner_train_mitie.py`

Use `python3 scripts/ner_train_mitie.py -h` to print cmd line arguments.

The resulting model will reside in `workspace/mitie/mitie_ner_model.dat`

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
