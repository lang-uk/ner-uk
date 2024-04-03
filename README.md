# NER-анотація українського корпусу

## Опис даних

**Це друга версія** українського NER корпусу. Дані першої версії та документацію ви можете знайти [тут](v1.0/)

Корпус розмічених даних знаходиться в папці `v2.0/data`.
Всього в корпусі:

- 262 текстів
- 237_327 токенів
- 7_441 сутностей NER
  - ПЕРС 4_387
  - ЛОК 1_614
  - ОРГ 780
  - РІЗН 660

Первинним джерелом даних є [відкритий корпус українських текстів](https://github.com/brown-uk/corpus) (папка [bruk](v2.0/data/bruk/)) та тексти видання «[Наші гроші](https://nashigroshi.org)» (папка [ng](v2.0/data/ng/)).
Для кожного обробленого тексту з корпусу наявні два файли:

- файл з розширенням `txt` містить токенізовану версію тексту
- файл з розширенням `ann` містить NER-анотації до цього тексту у форматі Brat Standoff Format (кожний рядок файлу містить 3 записи, розділені табуляцією: номер анотації, початковий і кінцевий індекс в тексті — у даному випадку, токенізованому — через пробіл, текст сутності)

Анотація виконана принаймні двома анотаторами на кожний текст за наступними [правилами](doc/README.md), розбіжності в результатах виправлені третім редактором.

Для тренування і валідації моделей рекомендовано використовувати [Стандартне розбиття на DEV і TEST набори](v2.0/dev-test-split.txt).

Репозиторій також містить [скрипти для конвертації даних](scripts/README.md#Converting-data-to-IOB/BEIOS-formats) у інші формати.


## Ліцензія

Ці дані доступні для використання згідно умов ліцензії "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License"

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/Dataset" property="dct:title" rel="dct:type">"Корпус NER-анотацій українських текстів"</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://github.com/lang-uk" property="cc:attributionName" rel="cc:attributionURL">lang-uk</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.<br />Based on a work at <a xmlns:dct="http://purl.org/dc/terms/" href="https://github.com/lang-uk/ner-uk" rel="dct:source">https://github.com/lang-uk/ner-uk</a>.


## To mention:
To realign a dataset after proof-reading:
```bash

python align_after_seva.py ../databank/dima/bruk ../databank/output/bruk
python align_after_seva.py ../databank/dima/ng ../databank/output/ng
```
To calculate stats on dev-test split:
```bash

python calculate_split_stats.py ../databank/dima/ ../databank/output/dev-test-split.txt
```

To export in IOB format with no nested tags:
```bash

python  convert_data.py --src_dataset ../databank/output --dst /tmp/ner-output --overlap_strategy remove_inner --split_file ../databank/output/dev-test-split.txt
```