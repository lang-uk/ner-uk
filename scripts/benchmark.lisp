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
  (float (/ (* 2 prec rec) (+ prec rec))))
  
(defun benchmark (gold-dir test-dir)
  (let ((total-gold 0.0)
        (total-test 0.0)
        (conf-mat #h(equal))
        tps tps2 tps3)
    (dolist (gold-file (directory (strcat gold-dir "*.ann")))
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
             (gold-begs #h())
             (gold-ends #h())
             (test-ends #h())
             (test-begs #h()))
        ;; match full
        (dotable (ent tag gold)
          (:+ (get# (pair tag :gold) conf-mat 0))
          (if-it (? test ent)
                 (if (eql tag it)
                     (progn (push ent tps)
                            (:+ (get# (pair tag tag) conf-mat 0)))
                     (progn (push (pair ent it) tps2)
                            (:+ (get# (pair tag it) conf-mat 0))))
                 (:= (? gold-begs @ent.beg) (pair ent tag)
                     (? gold-ends @ent.end) (pair ent tag))))
        (dotable (ent tag test)
          (:+ (get# (pair tag :test) conf-mat 0))
          (set# @ent.beg test-begs (pair ent tag))
          (set# @ent.end test-ends (pair ent tag)))
        ;; match part
        (dotable (beg ent-tag gold-begs)
          (when-it (? test-begs beg)
            (push (pair ent-tag it) tps3)
            (rem# (? ent-tag 0 'end) gold-ends)))
        (dotable (end ent-tag gold-ends)
          (when-it (? test-ends end)
            (push (pair ent-tag it) tps3)))
        (:+ total-gold (ht-count gold))
        (:+ total-test (ht-count test))))
    ;; calc stats
    (with ((tp (length tps))
           (tp2 (length tps2))
           (prec  (/ tp total-test))
           (rec   (/ tp total-gold))
           (prec2 (/ (+ tp tp2) total-test))
           (rec2  (/ (+ tp tp2) total-gold))
           (tp3 0)
           (tp4 0))
      ;; partial matches
      (dolist (tp tps3)
        (with ((((g-ent g-tag) (t-ent t-tag)) tp)
               (match% (/ (abs (- @g-ent.wc @t-ent.wc))
                          @g-ent.wc)))
          (if (eql g-tag t-tag)
              (progn (:+ tp3 match%)
                     (:+ (get# (pair g-tag g-tag) conf-mat 0) match%))
              (progn (:+ tp4 match%)
                     (:+ (get# (pair g-tag t-tag) conf-mat 0) match%)))))
      (let ((prec3 (/ (+ tp tp3) total-test))
            (rec3  (/ (+ tp tp3) total-gold))
            (prec4 (/ (+ tp tp2 tp3 tp4) total-test))
            (rec4  (/ (+ tp tp2 tp3 tp4) total-gold)))
        (print-conf-mat conf-mat)
        (values #h(:match-full-all (float (/ tp total-gold))
                   :match-full-span (float (/ (+ tp tp2) total-gold))
                   :match-part-full (float (/ (+ tp tp2 tp3) total-gold))
                   :match-part-span (float (/ (+ tp tp2 tp3 tp4) total-gold))
                   :prec-full-all prec
                   :rec-full-all rec
                   :prec-full-span prec2
                   :rec-full-span rec2
                   :prec-part-all prec3
                   :rec-part-all rec3
                   :prec-part-span prec4
                   :rec-part-span rec4
                   :f1-full (f1 prec rec)
                   :f1-span (f1 prec2 rec2)
                   :f1-part-full (f1 prec3 rec3)
                   :f1-part-span (f1 prec4 rec4))
                conf-mat
                (list tps tps2 tps3))))))

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
                       (- (? conf-mat (pair tag :gold))
                           (reduce '+ (mapcar ^(get# (pair tag %) conf-mat 0)
                                              tags)))))
                    tags))
    (dolist (test-tag (safe-sort tags '< :key ^(? conf-mat (pair % %))))
      (let ((total-cnt (? conf-mat (pair test-tag :test)))
            (matches-cnt (reduce '+ (mapcar ^(get# (pair % test-tag) conf-mat 0)
                                            tags))))
        (format t "~8<~A~>~C~8A~{~8A~}~%"
                test-tag #\Tab
                (fmt-int-or-float (- total-cnt matches-cnt))
                (mapcar ^(fmt-int-or-float (get# (pair % test-tag) conf-mat 0))
                        tags))))))
