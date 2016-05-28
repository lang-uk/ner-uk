(in-package :cl-user)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload :rutilsx)
  (use-package :rutilsx))

(named-readtables:in-readtable rutilsx:rutilsx-readtable)


;;; dev-test split

(defun split-dev-test (dir &key (dev% 0.7))
  (with ((ht #h())
         (files (mapcar 'pathname-name (directory (strcat dir "*.ann"))))
         (dev ())
         (test ()))
    (dolist (file files)
      (:+ (get# (? (pathname-name file) 0) ht 0)))
    (dotable (ch c ht)
      (with (((dev1 test1) (group (floor (* c dev%))
                                  (shuffle (keep ch files :key ^(? % 0))))))
        (appendf dev dev1)
        (appendf test test1)))
    (values dev test)))

(defun write-dev-test-file (file dir)
  (with-out-file (out file)
  (with ((dev test (split-dev-test dir)))
    (write-line "DEV" out)
    (dolist (file dev)
      (write-line file out))
    (terpri out)
    (write-line "TEST" out)
    (dolist (file test)
      (write-line file out)))))


;;; benchmark

(defstruct (ent (:print-object (lambda (obj stream)
                                 (format stream "<~@[~A ~]~A:~A ~A>"
                                         @obj.tag @obj.beg @obj.end @obj.text))))
  tag beg end (wc 1) text)

(defun to-ent (str)
  (with (((_ tag beg end &rest ent) (split-if 'white-char-p str)))
    (pair (make-ent :beg (parse-integer beg)
                    :end (parse-integer end)
                    :wc (length ent)
                    :text (strjoin #\Space ent))
          (mkeyw tag))))

(defun f1 (prec rec)
  (let ((div (+ prec rec)))
    (if (zerop div)
        0
        (float (/ (* 2 prec rec) div)))))

(defun add-words (ent tag begs &key bare)
  (let ((beg 0)
        (words (split #\Space @ent.text)))
    (when (rest words)
      (dolist (word words)
        (set# (+ @ent.beg beg) begs (pair (if bare (make-ent :text word) ent) tag))
        (:+ beg (1+ (length word)))))))

(defun benchmark (gold-dir test-dir &key only-files only-tag)
  (let ((total-gold 0.0)
        (total-test 0.0)
        (conf-mat0 #h(equalp))
        (conf-mat1 #h(equalp))
        tps1 tps2 tps3)
    (dolist (gold-file (directory (strcat gold-dir "*.ann")))
      (when (or (null only-files)
                (member (pathname-name gold-file) only-files :test 'string=))
        (with ((gold (pairs->ht (mapcar 'to-ent
                                        (split #\Newline (read-file gold-file)
                                               :remove-empty-subseqs t))
                                :test 'equalp))
               (test (pairs->ht (mapcar 'to-ent
                                        (split #\Newline
                                               (read-file
                                                (fmt "~A/~A.ann" test-dir
                                                     (pathname-name gold-file)))
                                               :remove-empty-subseqs t))
                                :test 'equalp))
               (gold-begs nil)
               (test-begs nil))
          (when only-tag
            (dotable (ent tag gold)
              (unless (eql tag only-tag)
                (rem# ent gold)))
            (dotable (ent tag test)
              (unless (eql tag only-tag)
                (rem# ent test))))
          ;; match full
          (let ((cur-begs #h()))
            (dotable (ent tag gold)
              (:+ (get# (pair tag :gold) conf-mat0 0))
              (:+ (get# (pair tag :gold) conf-mat1 0))
              (if-it (? test ent)
                     (progn
                       (if (eql tag it)
                           (push ent tps1)
                           (push (pair ent it) tps2))
                       (:+ (get# (pair tag it) conf-mat0 0))
                       (:+ (get# (pair tag it) conf-mat1 0)))
                     (add-words ent tag cur-begs)))
            (push cur-begs gold-begs))
          (let ((cur-begs #h()))
            (dotable (ent tag test)
              (:+ (get# (pair tag :test) conf-mat0 0))
              (:+ (get# (pair tag :test) conf-mat1 0))
              (add-words ent tag cur-begs :bare t))
            (push cur-begs test-begs))
          ;; match part
          (dolist (cur-begs test-begs)
            (let (part-tps)
              (dotable (beg ent-tag cur-begs)
                (dolist (alt-begs gold-begs)
                  (when-it (? alt-begs beg)
                    (push (pair it ent-tag) part-tps))))
              (when part-tps
                (push part-tps tps3))))
          (:+ total-gold (ht-count gold))
          (:+ total-test (ht-count test)))))
    ;; calc stats
    (with ((tp1 (length tps1))
           (tp2 (length tps2))
           (tp3 0)
           (tp4 0)
           (prec1 (/ tp1 total-test))
           (rec1  (/ tp1 total-gold))
           (prec2 (/ (+ tp1 tp2) total-test))
           (rec2  (/ (+ tp1 tp2) total-gold)))
      ;; partial matches
      (dolist (tps tps3)
        (dolist (tp tps)
          (with ((((g-ent g-tag) (t-ent t-tag)) tp)
                 (match% (- 1 (/ (abs (- @g-ent.wc @t-ent.wc))
                                 @g-ent.wc))))
;            (break "~A ~A: ~A" g-ent t-ent match%)
            (if (eql g-tag t-tag)
                (:+ tp3 match%)
                (:+ tp4 match%))
            (:+ (get# (pair g-tag t-tag) conf-mat1 0) match%))))
      (let ((prec3 (/ (+ tp1 tp3) total-test))
            (rec3  (/ (+ tp1 tp3) total-gold))
            (prec4 (/ (+ tp1 tp2 (length tps3)) total-test))
            (rec4  (/ (+ tp1 tp2 tp3 tp4) total-gold)))
        (values #h(:prec-1 prec1
                   :rec-1 rec1
                   :f1-1 (f1 prec1 rec1)
                   :prec-2 prec2
                   :rec-2 rec2
                   :f1-2 (f1 prec2 rec2)
                   :prec-3 prec3
                   :rec-3 rec3
                   :f1-3 (f1 prec3 rec3)
                   :prec-4 prec4
                   :rec-4 rec4
                   :f1-4 (f1 prec4 rec4))
                (list conf-mat0 conf-mat1)
                (list tps1 tps2 tps3))))))


;;; pretty printing

(defun fmt-int-or-float (num)
  (typecase num
    (integer (fmt "~8D" num))
    (rational (fmt "~8,2F" (float num)))
    (float (fmt "~8,2F" num))))

(defun print-conf-mat (conf-mat)
  (let (tags)
    (dotable (k _ conf-mat)
      (pushnew (lt k) tags))
    (:= tags (safe-sort tags '< :key ^(get# (pair % %) conf-mat 0)))
    (format t "        ~C~{~8<~A~>~}~%" #\Tab (cons nil tags))
    (format t "~8<~A~>~C        ~{~8A~}~%" nil #\Tab
            (mapcar (lambda (tag)
                      (fmt-int-or-float
                       (- (get# (pair tag :gold) conf-mat 0)
                           (reduce '+ (mapcar ^(get# (pair tag %) conf-mat 0)
                                              tags)))))
                    tags))
    (dolist (test-tag tags)
      (let ((total-cnt (get# (pair test-tag :test) conf-mat 0))
            (matches-cnt (reduce '+ (mapcar ^(get# (pair % test-tag) conf-mat 0)
                                            tags))))
        (format t "~8<~A~>~C~8A~{~8A~}~%"
                test-tag #\Tab
                (fmt-int-or-float (- total-cnt matches-cnt))
                (mapcar ^(fmt-int-or-float (get# (pair % test-tag) conf-mat 0))
                        tags))))))

(with ((dev-test (split #\Newline (read-file "../doc/dev-test-split.txt")
                        :remove-empty-subseqs t)))
  (defparameter *dev-data*
    (sub dev-test (1+ (position "DEV" dev-test :test 'string=))
         (position "TEST" dev-test :test 'string=)))
  (defparameter *test-data*
    (sub dev-test (1+ (position "TEST" dev-test :test 'string=)))))


(defun print-benchmark (gold-dir test-dir)
  (with ((qs ms (benchmark gold-dir test-dir :only-files *test-data*)))
    (loop :for (k v) :in (sort (ht->pairs qs) '<
                               :key ^(char-code (last-char (string (lt %))))) :do
      (format t "~:(~A~): ~5,3F~%" k v))
    (doindex (i m ms)
      (format t "~%Confusion matrix ~A:~%~%" (1+ i))
      (print-conf-mat m))))
