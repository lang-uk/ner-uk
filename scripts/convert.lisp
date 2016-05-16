(in-package :cl-user)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload :rutilsx)
  (ql:quickload :yason)
  (ql:quickload :cl-nlp)
  (use-package :rutilsx)
  (use-package :ncore)
  (use-package :nutil)
  (use-package :should-test))

(named-readtables:in-readtable rutilsx:rutilsx-readtable)

(defun json->ann (json-file dir &key (offset 40))
  (unless (ends-with "/" dir)
    (:= dir (strcat dir "/")))
  (let ((users #h(equal))
        (uid 0))
    (dolines (line json-file)
      (dolist (record (yason:parse line))
        (with ((((answer "answer") (task "task") (user "user")) ? record)
               (uid (getset# (? user "username") users (:+ uid)))
               (text (? answer "text"))
               (sent-spans (? answer "sentence_offsets"))
               (id (? answer "file_id")))
          (ensure-directories-exist (fmt "~A~A/" dir uid))
          (with-out-file (out (fmt "~A~A/~A.ann" dir uid id))
            (dolist (ner (sort (? answer "entities") '< :key ^(? % 2 0 0)))
              (with (((id type spans) ner)
                     ((beg end) (first spans))
                     (sent0 (position-if ^(<= (first %) beg (second %))
                                         sent-spans))
                     (sent-beg (first (? sent-spans sent0)))
                     (sent1 (position-if ^(<= (first %) end (second %))
                                         sent-spans))
                     (sent-end (last1 (? sent-spans sent1)))
                     (name (slice text beg end)))
                (when (> sent1 sent0)
                  (:= (? sent-spans sent0) (list sent-beg sent-end))
                  (removef sent-spans (? sent-spans sent1) :test 'equalp))
                (:- beg (- end beg (length name)))
                (format out "# ~A[[~A]]~A~%~A	~A ~A ~A	~A~%"
                        (slice text (or (position-if ^(member % '(#\Space #\Newline))
                                                     text
                                                     :start (max (- beg offset)
                                                                 sent-beg)
                                                     :end beg)
                                        beg)
                               beg)
                        name
                        (slice text end
                               (or (position-if ^(member % '(#\Space #\Newline))
                                                text :from-end t
                                                :start end
                                                :end (min (+ end offset) sent-end))
                                   end))
                        id type (+ beg sent0) (+ end sent1) name))))
          (dolist (outfile (list (fmt "~A~A/~A.txt" dir uid id)
                                 (fmt "~A~A.txt" dir id)))
            (with-out-file (out outfile)
              ;; (write-line text out)))
              (dolist (span sent-spans)
                (write-line (slice text (? span 0) (? span 1)) out)))))))
    (with-out-file (out (fmt "~Aannotators.txt" dir))
      (dotable (k v users)
        (format out "~A ~A~%" v k)))
    users))

(defparameter *tokenizer*
  (make 'regex-word-tokenizer
        :regex (re:create-scanner
                (strcat
                 ;; urls
                 "\\w+://\\S+"
                 ;; emails
                 "|[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+.[a-zA-Z0-9-.]+"
                 ;; 90-х etc.
                 "|[0-9]+-[а-яА-Я]{1,2}"
                 ;; decimals & ranges
                 "|[+-]?[0-9](?:[0-9,.-]*[0-9])?"
                 ;; regular words
                 "|[\\w]([\\w'’`-]?[\\w]+)*"
                 ;; abbreviations
                 "|\\w.(\\w.)+\\w?"
                 ;; punctuation & special symbols
                 "|[\"#$%&*+,/:;<=>@^`~…\\(\\)⟨⟩{}\\[\\|\\]‒–—―«»“”‘’']"
                 ;; closing punctuation
                 "|[.!?]+"
                 ;; multiple dashes
                 "|-+"))))

(defparameter +uk-abbrevs+
  (list-from-file (data-file "uk/abbrevs-with-dot.txt")))

(defun tokenize-text (file)
  (let ((text (read-file file))
        (+abbrevs-with-dot+ +uk-abbrevs+))
    (with-out-file (xml-out (strcat file ".xml"))
      (write-line "<root><body>" xml-out)
      (with-out-file (tok-out (strcat file ".tok"))
        (dolist (sent (flat-map ^(tokenize <sentence-splitter> %)
                                (split #\Newline
                                       (substr text (+ (search "<body>" text
                                                               :test 'string=)
                                                        6)
                                               (search "</body>" text
                                                       :test 'string=)))))
          (let ((toks (tokenize *tokenizer*
                                (re:regex-replace-all "</?[^>]+>" sent ""))))
            (write-line (strjoin #\Space toks)
                        tok-out)
            (write-line (strcat "<S>" (strjoin #\Space (mapcar ^(strcat % "[]")
                                                               toks))
                                "</S>")
                        xml-out))))
      (write-line "</body></root>" xml-out))))

(defun clean-up (file &key (dir :left))
  (let ((outfile (fmt "~A.2" file)))
    (with-out-file (out outfile)
      (dolines (line file)
        (with (((tn ner beg end &rest words) (split-if 'white-char-p line))
               (words (strjoin #\Space words))
               (beg (parse-integer beg))
               (end (parse-integer end))
               (fix (- (- end beg) (length words))))
          (format out "~A~C~A ~A ~A~C~A~%"
                  tn #\Tab ner
                  (ecase dir (:left (- beg fix)) (:right beg))
                  (ecase dir (:left end) (:right (- end fix)))
                  #\Tab words))))
    (rename-file outfile file)))

(defun validate-annotations (file)
  (assert (ends-with ".ann" (princ-to-string file)))
  (let ((text (read-file (fmt "~A.txt" (substr (princ-to-string file) 0 -4))))
        (i 1))
    (dolines (line file)
      (with (((tn ner beg end &rest word) (split-if 'white-char-p line))
             (expected (strjoin #\Space word))
             (actual (slice text (parse-integer beg) (parse-integer end))))
        (unless (string= actual expected)
          (format t "~A:~A - ~A - ~A~%" file i expected actual)))
      (:+ i)))
  (format t ".~%"))
