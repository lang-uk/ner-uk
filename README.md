# NER-анотація українського корпусу

## Опис даних

**Це друга версія** українського NER корпусу. Дані першої версії та документацію ви можете знайти [тут](v1.0/)

Корпус розмічених даних знаходиться в папці `v2.0/data`.
Всього в корпусі:

- 560 текстів (train: 391, test: 169)
- 21_993 сутностей NER:
|           | **NashiGroshi** | **Bruk** | **Total** |
|-----------|-----------------|----------|-----------|
| ART       |             319 |      316 |       635 |
| DATE      |            1496 |      551 |      2047 |
| DOC       |             108 |       34 |       142 |
| JOB       |            1344 |      638 |      1982 |
| LOC       |            1380 |     1620 |      3000 |
| MISC      |             102 |      413 |       515 |
| MON       |             897 |       46 |       943 |
| ORG       |            4431 |      782 |      5213 |
| PCT       |             186 |       77 |       263 |
| PERIOD    |             341 |      255 |       596 |
| PERS      |            1820 |     4415 |      6235 |
| QUANT     |             276 |      106 |       382 |
| TIME      |               4 |       36 |        40 |
| **Total** |       **12704** | **9289** | **21993** |

Первинним джерелом даних є [відкритий корпус українських текстів](https://github.com/brown-uk/corpus) (папка [bruk](v2.0/data/bruk/)) та тексти видання «[Наші гроші](https://nashigroshi.org)» (папка [ng](v2.0/data/ng/)).
Для кожного обробленого тексту з корпусу наявні два файли:

- файл з розширенням `txt` містить токенізовану версію тексту
- файл з розширенням `ann` містить NER-анотації до цього тексту у форматі Brat Standoff Format (кожний рядок файлу містить 3 записи, розділені табуляцією: номер анотації, початковий і кінцевий індекс в тексті — у даному випадку, токенізованому — через пробіл, текст сутності)

Анотація виконана принаймні двома анотаторами на кожний текст за наступними [правилами](doc/README.md), розбіжності в результатах виправлені третім редактором.

Для тренування і валідації моделей рекомендовано використовувати [Стандартне розбиття на DEV і TEST набори](v2.0/data/dev-test-split.txt).

Ми надаємо [сконвертовані у формат IOB дані](v2.0/iob/) з використанням стандартного розбиття. Під час цієї конвертації ми прибрали вкладені теги.

Репозиторій також містить [скрипти для конвертації даних](scripts/README.md#Converting-data-to-IOB/BEIOS-formats) у інші формати.


## Ліцензія

Ці дані доступні для використання згідно умов ліцензії "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License"

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/Dataset" property="dct:title" rel="dct:type">"Корпус NER-анотацій українських текстів"</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://github.com/lang-uk" property="cc:attributionName" rel="cc:attributionURL">lang-uk</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.<br />Based on a work at <a xmlns:dct="http://purl.org/dc/terms/" href="https://github.com/lang-uk/ner-uk" rel="dct:source">https://github.com/lang-uk/ner-uk</a>.
