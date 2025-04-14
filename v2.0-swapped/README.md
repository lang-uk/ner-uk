### Gender-swapped data corpus

**This is the gender-swapped version** of the [Ukrainian NER corpus](../v2.0/data).
The original corpus was filtered to retain only texts with gendered entities, such as JOB labels.

The labeled data corpus is located in the `v2.0-swapped/data` folder.

Total in the corpus:

- 405 texts (train: 283, test: 123)
- 5_978 NER entities
- 13 types of entities

| | **NashiGroshi** | **Bruk** | **Total** |
|----------- |----------------- |---------- |----------- |
| JOB             | 1344              | 638      | 1982      |
| PERS            | 1058              | 326      | 1384      |
| ART             | 28                | 39       | 67        |
| DATE            | 369               | 52       | 421       |
| DOC             | 16                | 9        | 25        |
| LOC             | 264               | 107      | 371       |
| MISC            | 15                | 23       | 38        |
| MON             | 118               | 0        | 118       |
| ORG             | 1261              | 117      | 1378      |
| PCT             | 48                | 1        | 49        |
| PERIOD          | 87                | 11       | 98        |
| QUANT           | 31                | 13       | 44        |
| TIME            | 1                 | 2        | 3         |
| **Total**       | **4640**           | **1338**  | **5978**  |

There are three files for each processed text from the corpus:

- a file with the extension `txt` contains the sentences that have been gender-swapped from the original text sentences. 
- a file with the extension `ann` contains NER-annotations for the gender-swapped text.
- a file with the extension `meta` maps each sentence in the gender-swapped file to its corresponding sentence in the original file by sentence index.

> **Note:** `filename-swapped` files correspond to the original `filename` files from the [Ukrainian NER corpus](../v2.0/data).

For model training and validation, we recommend using the [Standard split into DEV and TEST sets](data/dev-test-split.txt).
