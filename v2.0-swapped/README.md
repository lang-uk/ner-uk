### Gender-swapped data corpus

**This is the gender-swapped version** of the [Ukrainian NER corpus](../v2.0/data).
The original corpus was filtered to retain only texts with gendered entities, such as JOB labels.

The labeled data corpus is located in the `v2.0-swapped/data` folder.

Total in the corpus:

- 395 texts (train=248, dev=27, test=120)
- 5,385 NER entities
- 13 types of entities

| | **NashiGroshi** | **Bruk** | **Total** |
|----------- |----------------- |---------- |----------- |
| JOB             | 1248              | 485      | 1733      |
| PERS            | 1002              | 280      | 1282      |
| ART             | 25                | 23       | 48        |
| DATE            | 329               | 45       | 374       |
| DOC             | 12                | 6        | 18        |
| LOC             | 250               | 91      | 341       |
| MISC            | 14                | 21       | 35        |
| MON             | 108               | 0        | 108       |
| ORG             | 1167              | 100      | 1267      |
| PCT             | 48                | 0        | 48        |
| PERIOD          | 77                | 11       | 88        |
| QUANT           | 27                | 13       | 40        |
| TIME            | 1                 | 2        | 3         |
| **Total**       | **4308**           | **1077**  | **5385**  |

There are three files for each processed text from the corpus:

- a file with the extension `txt` contains the sentences that have been gender-swapped from the original text sentences. 
- a file with the extension `ann` contains NER-annotations for the gender-swapped text.
- a file with the extension `meta` maps each sentence in the gender-swapped file to its corresponding sentence in the original file by sentence index.

> **Note:** `filename-swapped` files correspond to the original `filename` files from the [Ukrainian NER corpus](../v2.0/data).

For model training and validation, we recommend using the [Standard split into DEV and TEST sets](data/dev-test-split.txt).
