;;; 2016 (c) Vsevolod Dyomkin <vseloved@gmail.com>

(defpackage #:tokenize-uk
  (:use #:cl)
  (:export #:tokenize-text
           #:tokenize-sents
           #:tokenize-words
           #:tokenize-file
           #:write-tokenized))

(in-package #:tokenize-uk)

(defun split (char str)
  (loop :for left := 0 :then (+ right 1)
        :for right := (or (position char str :start left)
                          (length str))
        :unless (= right left)
          :collect (subseq str left right) :into subseqs
        :until (>= right (length str))
        :finally (return subseqs)))


(defparameter *sent-end-chars* '(#\. #\? #\! #\… #\¶ #\»))

(defparameter *abbrevs-with-dot* (split #\Newline "
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
"))

(defparameter *word-tokenization-rules*
  (ppcre:create-scanner
   (concatenate 'string
    ;; urls
    "\\w+://(?:[a-zA-Z]|[0-9]|[$-_@.&+])+"
    ;; emails
    "|[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+.[a-zA-Z0-9-.]+"
    ;; 90-х etc.
    "|[0-9]+-[а-яА-ЯіїІЇ'’`]+"
    ;; decimals & ranges
    "|[+-]?[0-9](?:[0-9,.-]*[0-9])?"
    ;; regular words
    "|[\\w](?:[\\w'’`-]?[\\w]+)*"
    ;; abbreviations
    "|\\w.(?:\\w.)+\\w?"
    ;; punctuation & special symbols
    "|[\"#$%&*+,/:;<=>@^`~…\\(\\)⟨⟩{}\\[\\|\\]‒–—―«»“”‘’'№]"
    ;; closing punctuation
    "|[.!?]+"
    ;; multiple dashes
    "|-+")))

(defstruct (span (:conc-name nil)) beg end)

(defun regex-tokenize (regex str)
  (loop :for (beg end) :on (ppcre:all-matches regex str)
        :by #'cddr
        :collect (subseq str beg end) :into words
        :collect (make-span :beg beg :end end) :into spans
        :finally (return (values words
                                 spans))))

(defun tokenize-words (str)
  (regex-tokenize *word-tokenization-rules* str))

(defun tokenize-sents (str)
  (multiple-value-bind (words word-spans) (regex-tokenize "[^\\s]+" str)
    (let ((beg 0)
          sents spans)
      (loop :for ws :on words :and ss :on word-spans :do
        (let* ((word (first ws))
               (span (first ss))
               (last-char (char word (1- (length word)))))
          (when (or (null (rest ws))
                    (and (member last-char *sent-end-chars*)
                         (notevery #'upper-case-p
                                   (subseq word 0
                                           (position-if
                                            (lambda (char)
                                              (member char *sent-end-chars*))
                                            word)))
                         (or (not (char= #\. last-char))
                             (not (or (char= #\( (char word 0))
                                      (member word *abbrevs-with-dot*
                                              :test 'string=))))
                         (upper-case-p (char (second ws) 0))))
            (push (subseq str beg (end span)) sents)
            (push (make-span :beg beg :end (end span)) spans)
            (when (second ss) (setf beg (beg (second ss)))))))
      (values (reverse sents)
              (reverse spans)))))

(defun tokenize-text (str)
  (mapcar (lambda (par)
            (mapcar 'tokenize-words
                    (tokenize-sents par)))
          (split #\Newline str)))

(defun write-tokenized (str &optional (out *standard-output*))
  (write-line (format nil "~{~A~^~%~}"
                       (mapcar (lambda (sents)
                                 (format nil "~{~A~%~}"
                                          (mapcar (lambda (toks)
                                                    (format nil "~{~A~^ ~}" toks))
                                                  sents)))
                               (tokenize-text str)))
              out))
                   
(defun tokenize-file (file &optional (outfile (concatenate 'string file ".tok")))
  (with-open-file (out outfile :direction :output
                               :if-exists :supersede :if-does-not-exist :create)
    (with-open-file (in file)
      (loop :for line := (read-line in nil) :while line :do
        (write-tokenized line out))))
  (format t "Wrote tokenized file ~A.tok~%" file) (finish-output)
  outfile)

(defun main (argv)
  (if (zerop (length argv))
      (loop (write-tokenized (read-line)))
      (dolist (file argv)
        (tokenize-file file))))
  
(eval-when (:execute)
  (main
   #+sbcl (let ((--pos (position "--" sb-ext:*posix-argv* :test 'string=)))
            (when --pos
              (subseq sb-ext:*posix-argv* (1+ --pos))))))
