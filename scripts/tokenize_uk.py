#!env python

##### Ukrainian tokenization script based on
##### [standard tokenization algorithm](https://github.com/lang-uk/ner-uk/blob/master/doc/tokenization.md)
##### 2016 (c) Vsevolod Dyomkin <vseloved@gmail.com>

##### Usage: ./tokenize_uk.py file
##### Result: tokenized data in file.tok

import sys
import re

def tokenize_text(string):
    rez = []
    for part in string.split('\n'):
        par = []
        for sent in tokenize_sents(part):
            par.append(tokenize_words(string))
        rez.append(par)
    return rez

ABBRS = """
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
        elif tok[-1] in ['.', '!', '?', '…', '»']:
            tok1 = tok[re.search('[.!?…»]', tok).start()]
            next_tok = string[spans[i+1].start():spans[i+1].end()]
            if (next_tok[0].isupper()
                and not tok1.isupper()
                and not (not tok[-1] == '.'
                         or (tok1[0] == '('
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
""", re.X)

def tokenize_words(string):
    return re.findall(WORD_TOKENIZATION_RULES, string)
    
if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: ./tokenize_uk.py file")
    else:
        with open(sys.argv[1], 'r') as in_f:
            with open(sys.argv[1] + '.tok', 'w') as out_f:
                for line in in_f:
                    for sent in tokenize_sents(line):
                        for word in tokenize_words(sent):
                            out_f.write(word + ' ')
                        out_f.write('\n')
                    out_f.write('\n')
                
