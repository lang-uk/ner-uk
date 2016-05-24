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
  tag beg end wc text)

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

(defun add-words (ent tag begs)
  (let ((beg 0))
    (dolist (word (split #\Space @ent.text))
      (set# (+ @ent.beg beg) begs (pair ent tag))
      (:+ beg (1+ (length word))))))

(defun benchmark (gold-dir test-dir &key only-files verbose only-tag)
  (let ((total-gold 0.0)
        (total-test 0.0)
        (conf-mat #h(equal))
        tps1 tps2 tps3)
    (dolist (gold-file (directory (strcat gold-dir "*.ann")))
      (when (or (null only-files)
                (member (pathname-name gold-file) only-files :test 'string=))
        (when verbose
          (princ gold-file) (terpri))
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
          (when verbose
            (print gold)
            (print test))
          ;; match full
          (let ((cur-begs #h()))
            (dotable (ent tag gold)
              (:+ (get# (pair tag :gold) conf-mat 0))
              (if-it (? test ent)
                     (if (eql tag it)
                         (progn
                           (when verbose (print ent))
                           (push ent tps1)
                           (:+ (get# (pair tag tag) conf-mat 0)))
                         (progn
                           (push (pair ent it) tps2)
                           (:+ (get# (pair tag it) conf-mat 0))))
                     (add-words ent tag cur-begs)))
            (push cur-begs gold-begs))
          (let ((cur-begs #h()))
            (dotable (ent tag test)
              (:+ (get# (pair tag :test) conf-mat 0))
              (add-words ent tag cur-begs))
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
          (:+ total-test (ht-count test)))
        (when verbose
          (print (list (length tps1) (length tps2) (length tps3)))
          (break))))
    ;; calc stats
    (with ((tp1 (length tps1))
           (tp2 (length tps2))
           (prec  (/ tp1 total-test))
           (rec   (/ tp1 total-gold))
           (prec2 (/ (+ tp1 tp2) total-test))
           (rec2  (/ (+ tp1 tp2) total-gold))
           (tp3 0)
           (tp4 0))
      ;; partial matches
      (dolist (tps tps3)
        (dolist (tp tps)
          (with ((((g-ent g-tag) (t-ent t-tag)) tp)
                 (match% (/ (abs (- @g-ent.wc @t-ent.wc))
                            @g-ent.wc)))
            (if (eql g-tag t-tag)
                (progn
                  (:+ tp3 match%)
                  (:+ (get# (pair g-tag g-tag) conf-mat 0) match%))
                (progn
                  (:+ tp4 match%)
                  (:+ (get# (pair g-tag t-tag) conf-mat 0) match%))))))
      (let ((prec3 (/ (+ tp1 tp3) total-test))
            (rec3  (/ (+ tp1 tp3) total-gold))
            (prec4 (/ (+ tp1 tp2 (length tps3)) total-test))
            (rec4  (/ (+ tp1 tp2 tp3 tp4) total-gold)))
        (print-conf-mat conf-mat)
        (values #h(:prec-full-match prec
                   :rec-full-match rec
                   :f1-full-match (f1 prec rec)
                   
                   :prec-full-match-no-tags prec2
                   :rec-full-match-no-tags rec2
                   :f1-full-match-no-tags (f1 prec2 rec2)
                   
                   :prec-part-match prec3
                   :rec-part-match rec3
                   :f1-part-match (f1 prec3 rec3)
                   
                   :prec-part-match-no-tags prec4
                   :rec-part-match-no-tags rec4
                   :f1-part-match-no-tags (f1 prec4 rec4))
                conf-mat
                (list tps1 tps2 tps3))))))

(defun fmt-int-or-float (num)
  (typecase num
    (integer (fmt "~8D" num))
    (rational (fmt "~8,2F" (float num)))
    (float (fmt "~8,2F" num))))

(defun print-conf-mat (conf-mat)
  (let (tags)
    (dotable (k _ conf-mat)
      (pushnew (lt k) tags))
    (format t "        ~C~{~8<~A~>~}~%" #\Tab (cons nil tags))
    (format t "~8<~A~>~C        ~{~8A~}~%" nil #\Tab
            (mapcar (lambda (tag)
                      (fmt-int-or-float
                       (- (get# (pair tag :gold) conf-mat 0)
                           (reduce '+ (mapcar ^(get# (pair tag %) conf-mat 0)
                                              tags)))))
                    tags))
    (dolist (test-tag (safe-sort tags '< :key ^(get# (pair % %) conf-mat 0)))
      (let ((total-cnt (get# (pair test-tag :test) conf-mat 0))
            (matches-cnt (reduce '+ (mapcar ^(get# (pair % test-tag) conf-mat 0)
                                            tags))))
        (format t "~8<~A~>~C~8A~{~8A~}~%"
                test-tag #\Tab
                (fmt-int-or-float (- total-cnt matches-cnt))
                (mapcar ^(fmt-int-or-float (get# (pair % test-tag) conf-mat 0))
                        tags))))))
