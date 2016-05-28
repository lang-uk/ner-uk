(in-package :cl-user)
(named-readtables:in-readtable rutilsx:rutilsx-readtable)


;;; tagger

(defclass ap-nertagger (avg-perceptron tagger)
  ((pad :initarg :pad :initform 2 :reader tagger-pad))
  (:documentation
   ""))

(defclass bi-ap-nertagger ()
  ((dir-tagger :initarg :dir :accessor tagger-dir-tagger)
   (rev-tagger :initarg :rev :accessor tagger-rev-tagger)
   ;;   (pad))
   )
  (:documentation
   ""))

(defmacro with-nertagger-init ((tagger sentence) &body body)
  `(let ((ctx #h(:sent (make-array
                        (+ (length (sent-tokens ,sentence))
                           (* (? ,tagger 'pad) 2))
                        :initial-contents
                        (append (mapcar ^(make-entity :word "" :ner %)
                                        (subseq '(:-start3- :-start2- :-start-)
                                                (- 3 (? ,tagger 'pad))))
                                (sent-tokens ,sentence)
                                (mapcar ^(make-entity :word "" :ner %)
                                        (sub '(:-end- :-end2- :-end3-)
                                             0 (? ,tagger 'pad))))))))
     (:= (? ctx :prev) :-start-
         (? ctx :prev2) :-start2-
         (? ctx :prev3) :-start3-)
     ,@body))

(defmethod tag ((tagger ap-nertagger) (sentence sentence))
  (with-nertagger-init (tagger sentence)
    (doindex (i token @sentence.tokens)
      (:= @token.ner (classify tagger (extract-fs tagger (+ i @tagger.pad) ctx))))
    sentence))

(defmethod tag ((tagger bi-ap-nertagger) (sentence sentence))
  (loop :for tok :in @sentence.tokens
        :for d :in
        (let ((tagger @tagger.dir-tagger))
          (with-nertagger-init (tagger sentence)
            (mapindex ^(rank tagger
                             (extract-fs tagger (+ % @tagger.pad) ctx))
                      @sentence.tokens)))
        :for r :in
        (let ((+open-quote-chars+ +close-quote-chars+)
              (+close-quote-chars+ +open-quote-chars+)
              (tagger @tagger.rev-tagger)
              (tokens (reverse @sentence.tokens)))
          (with-nertagger-init (tagger (make 'sentence :tokens tokens))
            (reverse (mapindex ^(rank tagger
                                      (extract-fs tagger (+ % @tagger.pad) ctx))
                               tokens))))
        :do ;(break "~A ~A ~A" tok d r)
            (:= @tok.ner
                (lt (reduce ^(if (> (rt %) (rt %%)) % %%)
                            (mapcar ^(pair (lt %) (max (rt %) (rt %%)))
                                    (sort (ht->pairs d) 'string< :key 'lt)
                                    (sort (ht->pairs r) 'string< :key 'lt))))))
  sentence)

(defmethod train ((model ap-nertagger) sents &key (epochs 5) verbose)
  (training-perceptron (sent sents epochs verbose c n)
    (with-nertagger-init (model sent)
      (doindex (i token @sent.tokens)
        (with ((fs (extract-fs model (+ i @model.pad) ctx))
               (guess (classify model fs)))
          (train1 model @token.ner guess (rest fs))
          (:= (? ctx :prev3) (? ctx :prev2)
              (? ctx :prev2) (? ctx :prev)
              (? ctx :prev) guess)
          (switch (@token.word#0 :test 'member)
            (+open-quote-chars+ (:= (? ctx :quoted) t))
            (+close-quote-chars+ (:= (? ctx :quoted) nil)))
          (when verbose
            (:+ c (if (eql guess @token.ner) 1 0))
            (:+ n))))))
  model)

(defun latin-char-p (char)
  (and (member char
               (load-time-value
                (append (loop :for ch :from (char-code #\a) :to (char-code #\z)
                              :collect (code-char ch))
                        (loop :for ch :from (char-code #\A) :to (char-code #\Z)
                              :collect (code-char ch)))))
       t))

