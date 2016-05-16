# NER-анотація українського корпусу

## Опис даних

Корпус розмічених даних знаходиться в папці `doc`.
Всього в корпусі: 31 текст, які містять ??? токенів, ??? сутносткй NER.

Первинним джерелом даних є [відкритий корпус українських текстів]().
Для кожного обробленого тексту з корпусу наявні два файли:

- файл з рошриенням `tok.txt` містить токенізовану версію тексту (токенізація зроблена за наступними [правилами](../doc/tokenization.md))
- файл з розширенням `tok.ann` містить NER-анотації до цього тексту у форматі Brat Standoff Format (кожний рядок файлу містить 3 записи, розділені табуляцією: номер анотації, початковий і кінцевий індекс в тексті — у даному випадку, токенізованому — через пробіл, текст сутності)

Анотація виконана двома анотаторами на кожний текст за наступними [правилами](../doc/README.md), розбіжності в результатах виправлені третім анотатором

## Ліцензія

Ці дані доступні для використання згідно умов ліцензії "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License"

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/Dataset" property="dct:title" rel="dct:type">"Корпус NER-анотацій українських текстів"</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://github.com/lang-uk" property="cc:attributionName" rel="cc:attributionURL">lang-uk</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.<br />Based on a work at <a xmlns:dct="http://purl.org/dc/terms/" href="https://github.com/lang-uk/ner-uk" rel="dct:source">https://github.com/lang-uk/ner-uk</a>.
