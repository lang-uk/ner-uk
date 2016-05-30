# NER-UK Scripts

## tokenize-uk.lisp

### Installation:

```
$ sbcl --non-interactive --eval '(compile-file "tokenize-uk--all-systems.lisp")'
```

### Usage:

- tokenize text from STDIN line-by-line:

    ```
    $ sbcl --script tokenize-uk--all-systems.fasl
    ```

- tokenize files (for each file create a corresponding .tok file):

    ```
    $ sbcl --script tokenize-uk--all-systems.fasl -- file-to-tokenize1 file-to-tokenize2 ...
    ```
