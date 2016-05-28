#!env python
# -*- coding: utf-8 -*-

##### Ukrainian tokenization script based on
##### [standard tokenization algorithm](https://github.com/lang-uk/ner-uk/blob/master/doc/tokenization.md)
##### 2016 (c) Vsevolod Dyomkin <vseloved@gmail.com>

##### Usage: ./tokenize_uk.py file
##### Result: tokenized data in file.tok

import sys
import re
import io

def tokenize_text(string):
    rez = []
    for part in string.split(u'\n'):
        par = []
        for sent in tokenize_sents(part):
            par.append(tokenize_words(string))
        rez.append(par)
    return rez

ABBRS = u"""
ім.
о.
вул.
просп.
бул.
пров.
пл.
г.
р.
див.
п.
с.
м.
""".strip().split()

def tokenize_sents(string):
    spans = []
    for match in re.finditer('[^\s]+', string):
        spans.append(match)
    spans_count = len(spans)

    rez = []
    off = 0

    for i in range(spans_count):
        tok = string[spans[i].start():spans[i].end()]
        if i == spans_count - 1:
            rez.append(string[off:spans[i].end()])
        elif tok[-1] in [u'.', u'!', u'?', u'…', u'»']:
            tok1 = tok[re.search(u'[.!?…»]', tok).start()]
            next_tok = string[spans[i+1].start():spans[i+1].end()]
            if (next_tok[0].isupper()
                and not tok1.isupper()
                and not (not tok[-1] == u'.'
                         or (tok1[0] == u'('
                             or tok in ABBRS))):
                rez.append(string[off:spans[i].end()])
                off = spans[i+1].end()

    return rez

WORD_TOKENIZATION_RULES = re.compile(r"""
\w+://(?:[a-zA-Z]|[0-9]|[$-_@.&+])+
|[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+.[a-zA-Z0-9-.]+
|[0-9]+-[а-яА-ЯіїІЇ'’`]+
|[+-]?[0-9](?:[0-9,.-]*[0-9])?
|[\w](?:[\w'’`-]?[\w]+)*
|\w.(?:\\w.)+\w?
|["#$%&*+,/:;<=>@^`~…\\(\\)⟨⟩{}\[\|\]‒–—―«»“”‘’'№]
|[.!?]+
|-+
""", re.X|re.U)

def tokenize_words(string):
    return re.findall(WORD_TOKENIZATION_RULES, string)
    
if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: ./tokenize_uk.py file")
    else:
        with io.open(sys.argv[1], mode='r', encoding='utf-8') as in_f:
            with io.open(sys.argv[1] + '.tok', mode='w', encoding='utf-8') as out_f:
                for line in in_f:
                    for sent in tokenize_sents(line):
                        for word in tokenize_words(sent):
                            out_f.write(word + u' ')
                        out_f.write(u'\n')
                    out_f.write(u'\n')
                
