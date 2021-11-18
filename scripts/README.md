# NER-UK Scripts

## Training NER models

Pre-requisites
* `pip3 install -r scripts/requirements.txt`

Need IOB/BIO formatted files? Run this: `python3 scripts/convert_data.py`
Files will be written to `workspace/data` folder.

### MITIE
Requirements:
* trained feature extractor - binary can be obtained by running `MITIE/tools/wordrep` on data set. If not provided, script will try to download pretrained model.

Run from project root
`python3 scripts/train_mitie_ner.py`

Use `python3 scripts/ner_train_mitie.py -h` to print cmd line arguments.

The resulting model will reside in `workspace/mitie/mitie_ner_model.dat`

## Stanza
Stanza already have almost everything that is required to train on our data set. But if data set was updated - you can execute training process with: `scripts/train_stanza_ner.sh`

To use custom word vectors, specify its file path with argument `--word_vec` or `-w`. **Stanza uses vectors of dimension `100`**. 
`scripts/train_stanza_ner.sh -w=path_to_your_wordvec_file`

Vector file must be converted to .pt format (pytorch binary format) using script from stanza `convert_pretrain.py`. More details please read [here in Stanza docs](https://stanfordnlp.github.io/stanza/word_vectors.html).
Vector file to be converted - must start with line of format: `<word_count> <dimension [100]>`

Word count can usually be obtained by `wc -l <file>`

Trained model will be available under workspace/stanza folder.

Stanza additionally provides pretrained Character level model. It was shown to improve performance of the NER model by around 1%.

## Evaluating NER models
Execute: `python3 scripts/eval_ner_models.py --stanza=<path_to_stanza_model> --mitie=<path_to_mitie_model>`

The script will produce report with the help of sklearn classification report and will print it out to console.

## Converting data to IOB/BEIOS formats
If you need data in IOB/BEIOS formats for some other kinds of training just use `convert_data.py` script.

`python3 scripts/convert_data.py` will generate iob files according to train/test split in doc/dev-test-split.txt.
For more parameters run `python3 scripts/convert_data.py -h`.

## Running tests
```shell
python3 -m unittest discover -s test
```

## tokenize-uk.lisp


### Installation for other scripts

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
