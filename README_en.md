# NER-UK 2.0: A Rich Corpus of Named Entities for Ukrainian
[Українська версія](README.md)


## Data description.

**This is the second version** of the Ukrainian NER corpus. You can find the first version data and documentation [here](v1.0/)

The labeled data corpus is located in the `v2.0/data` folder.
Total in the corpus:

- 560 texts (train: 391, test: 169)
- 21_993 NER entities
- 13 types of entities

| | **NashiGroshi** | **Bruk** | **Total** |
|----------- |----------------- |---------- |----------- |
| ART | 319 | 316 | 635 |
| DATE | 1496 | 551 | 2047 |
| DOC | 108 | 34 | 142 |
| JOB | 1344 | 638 | 1982 |
| LOC | 1380 | 1620 | 3000 |
| MISC | 102 | 413 | 515 |
| MON | 897 | 46 | 943 |
| ORG | 4431 | 782 | 5213 |
| PCT | 186 | 77 | 263 |
| PERIOD | 341 | 255 | 596 |
| PERS | 1820 | 4415 | 6235 |
| QUANT | 276 | 106 | 382 |
| TIME | 4 | 36 | 40 |
**Total** | **12704** | **9289** | **21993** |

The primary data source is the [Open Corpus of Ukrainian Texts](https://github.com/brown-uk/corpus) (folder [bruk](v2.0/data/bruk/) and the texts of the publication "[Nashi Groshi](https://nashigroshi.org)" (folder [ng](v2.0/data/ng/)).
There are two files for each processed text from the corpus:

- a file with the extension `txt` contains the tokenized version of the text
- a file with the extension `ann` contains NER-annotations to this text in Brat Standoff Format (each line of the file contains 3 records separated by tabs: the annotation number, the start and end index in the text - in this case, the tokenized one - separated by a space, the entity text)

The annotation was performed by at least two annotators for each text according to the following [rules] (doc/README.md), with discrepancies in the results corrected by a third editor.

For model training and validation, we recommend using the [Standard split into DEV and TEST sets](v2.0/data/dev-test-split.txt).

We provide [IOB-converted data](v2.0/iob/) using the standard breakdown. Під час цієї конвертації ми прибрали вкладені теги.

The repository also contains [scripts for converting data](scripts/README.md#Converting-data-to-IOB/BEIOS-formats) to other formats.


## License

This data is available for use under the terms of the "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License"

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/Dataset" property="dct:title" rel="dct:type">"Корпус NER-анотацій українських текстів"</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://github.com/lang-uk" property="cc:attributionName" rel="cc:attributionURL">lang-uk</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.<br />Based on a work at <a xmlns:dct="http://purl.org/dc/terms/" href="https://github.com/lang-uk/ner-uk" rel="dct:source">https://github.com/lang-uk/ner-uk</a>.
