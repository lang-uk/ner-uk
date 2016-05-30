# NER-UK Scripts

## tokenize-uk.lisp

Installation:

```
$ sbcl --non-interactive --eval '(compile-file "tokenize-uk--all-systems.lisp")'
```

Usage:

```
$ sbcl --script tokenize-uk--all-systems.fasl
```
Tokenizes text from STDIN line-by-line.

```
$ sbcl --script tokenize-uk--all-systems.fasl -- file-to-tokenize
```
Produces file-to-tokenize.tok
