;;;; TOKENIZE-UK system definition
;;;; 2016 (c) Vsevolod Dyomkin

(asdf:defsystem #:tokenize-uk
  :version "0.1.0"
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description "Robust rule-based tokenization for Ukrainian language."
  :depends-on (#:cl-ppcre)
  :components ((:file "tokenize-uk")))
