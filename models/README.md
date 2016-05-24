# Тестування моделей

## Методологія тестування

- використовується [стандартне dev-test розбиття](../doc/dev-test-split.txt)
- рахується якість по 4-м параметрам:
  - повне співпадіння (1)
  - повне співпадіння діапазонів без врахування тегів (2)
  - часткове співпадіння: якщо щнайдена частина сутності, то для precision зараховується як 1 TP, а для recall — як TP на той відсоток, який співпав (3)
  - часткове співпадіння без врахування тегів (4)


## Журнал тестування

### [2016-05-24] MITIE model

```
    	     NIL    РІЗН     ОРГ     ЛОК    ПЕРС
 NIL	           79.83   38.98   58.67  169.18
РІЗН	   21.09   73.39   58.43    6.33    7.75
 ОРГ	     -11.29   14.84  108.32   21.13       8
 ЛОК	      31.43   21.64   40.77  327.37   18.80
ПЕРС	   89.43   30.30   16.50   47.50 1138.27
 
1
prec-full-match - 0.73816425
rec-full-match - 0.66841644
f1-full-match - 0.70156103

2
prec-full-match-no-tags - 0.8347826
rec-full-match-no-tags - 0.7559055
f1-full-match-no-tags - 0.7933884

3
prec-part-match - 0.79581517
rec-part-match - 0.72062004
f1-part-match - 0.7563533

4
prec-part-match-no-tags - 0.8570048
rec-part-match-no-tags - 0.8483516
f1-part-match-no-tags - 0.8526563
```

### [2016-05-24] AP model local words+tags features

```
    	     NIL    РІЗН     ОРГ     ЛОК    ПЕРС
 NIL	           99.54   46.26  126.10  201.70
РІЗН	   10.95   11.97   13.50    2.83    0.75
 ОРГ	      21.31    1.86   31.53    6.30       1
 ЛОК	      21.22    7.13   35.01  135.13    0.50
ПЕРС	  466.12   99.50  136.70  190.63 1138.05

1
prec-full-match - 0.41595197
rec-full-match - 0.42432195
f1-full-match - 0.42009526

2
prec-full-match-no-tags - 0.5497427
rec-full-match-no-tags - 0.5608049
f1-full-match-no-tags - 0.55521876

3
prec-part-match - 0.56461614
rec-part-match - 0.5759776
f1-part-match - 0.57024026

4
prec-part-match-no-tags - 0.57375646
rec-part-match-no-tags - 0.79282546
f1-part-match-no-tags - 0.6657321

