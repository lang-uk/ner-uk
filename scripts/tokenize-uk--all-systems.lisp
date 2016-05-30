;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/packages.lisp,v 1.39 2009/09/17 19:17:31 edi Exp $

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)

(defpackage :cl-ppcre
  (:nicknames :ppcre)
  #+:genera
  (:shadowing-import-from :common-lisp :lambda :simple-string :string)
  (:use #-:genera :cl #+:genera :future-common-lisp)
  (:shadow :digit-char-p :defconstant)
  (:export :parse-string
           :create-scanner
           :create-optimized-test-function
           :parse-tree-synonym
           :define-parse-tree-synonym
           :scan
           :scan-to-strings
           :do-scans
           :do-matches
           :do-matches-as-strings
           :all-matches
           :all-matches-as-strings
           :split
           :regex-replace
           :regex-replace-all
           :regex-apropos
           :regex-apropos-list
           :quote-meta-chars
           :*regex-char-code-limit*
           :*use-bmh-matchers*
           :*allow-quoting*
           :*allow-named-registers*
           :*optimize-char-classes*
           :*property-resolver*
           :ppcre-error
           :ppcre-invocation-error
           :ppcre-syntax-error
           :ppcre-syntax-error-string
           :ppcre-syntax-error-pos
           :register-groups-bind
           :do-register-groups))
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/specials.lisp,v 1.43 2009/10/28 07:36:15 edi Exp $

;;; globally declared special variables

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

;;; special variables used to effect declarations

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *standard-optimize-settings*
    '(optimize
      speed
      (safety 0)
      (space 0)
      (debug 1)
      (compilation-speed 0)
      #+:lispworks (hcl:fixnum-safety 0))
    "The standard optimize settings used by most declaration expressions.")

  (defvar *special-optimize-settings*
    '(optimize speed space)
    "Special optimize settings used only by a few declaration expressions.")
  )

;;; special variables used by the lexer/parser combo

(defvar *extended-mode-p* nil
  "Whether the parser will start in extended mode.")
(declaim (boolean *extended-mode-p*))

;;; special variables used by the SCAN function and the matchers

(defvar *regex-char-code-limit* char-code-limit'
  "The upper exclusive bound on the char-codes of characters which can
occur in character classes.  Change this value BEFORE creating
scanners if you don't need the \(full) Unicode support of
implementations like AllegroCL, CLISP, LispWorks, or SBCL.")
(declaim (fixnum *regex-char-code-limit*))
  
(defvar *string* (make-sequence #+:lispworks 'lw:simple-text-string
                                #-:lispworks 'simple-string
                                0)
  "The string which is currently scanned by SCAN.
Will always be coerced to a SIMPLE-STRING.")
#+:lispworks
(declaim (lw:simple-text-string *string*))
#-:lispworks
(declaim (simple-string *string*))

(defvar *start-pos* 0
  "Where to start scanning within *STRING*.")
(declaim (fixnum *start-pos*))

(defvar *real-start-pos* nil
  "The real start of *STRING*. This is for repeated scans and is only used internally.")
(declaim (type (or null fixnum) *real-start-pos*))

(defvar *end-pos* 0
  "Where to stop scanning within *STRING*.")
(declaim (fixnum *end-pos*))

(defvar *reg-starts* (make-array 0)
  "An array which holds the start positions
of the current register candidates.")
(declaim (simple-vector *reg-starts*))
  
(defvar *regs-maybe-start* (make-array 0)
  "An array which holds the next start positions
of the current register candidates.")
(declaim (simple-vector *regs-maybe-start*))

(defvar *reg-ends* (make-array 0)
  "An array which holds the end positions
of the current register candidates.")
(declaim (simple-vector *reg-ends*))

(defvar *end-string-pos* nil
  "Start of the next possible end-string candidate.")

(defvar *rep-num* 0
  "Counts the number of \"complicated\" repetitions while the matchers
are built.")
(declaim (fixnum *rep-num*))

(defvar *zero-length-num* 0
  "Counts the number of repetitions the inner regexes of which may
have zero-length while the matchers are built.")
(declaim (fixnum *zero-length-num*))

(defvar *repeat-counters* (make-array 0
                                      :initial-element 0
                                      :element-type 'fixnum)
  "An array to keep track of how often
repetitive patterns have been tested already.")
(declaim (type (array fixnum (*)) *repeat-counters*))

(defvar *last-pos-stores* (make-array 0)
  "An array to keep track of the last positions
where we saw repetitive patterns.
Only used for patterns which might have zero length.")
(declaim (simple-vector *last-pos-stores*))

(defvar *use-bmh-matchers* nil
  "Whether the scanners created by CREATE-SCANNER should use the \(fast
but large) Boyer-Moore-Horspool matchers.")

(defvar *optimize-char-classes* nil
  "Whether character classes should be compiled into look-ups into
O\(1) data structures.  This is usually fast but will be costly in
terms of scanner creation time and might be costly in terms of size if
*REGEX-CHAR-CODE-LIMIT* is high.  This value will be used as the :KIND
keyword argument to CREATE-OPTIMIZED-TEST-FUNCTION - see there for the
possible non-NIL values.")

(defvar *property-resolver* nil
  "Should be NIL or a designator for a function which accepts strings
and returns unary character test functions or NIL.  This 'resolver' is
intended to handle `character properties' like \\p{IsAlpha}.  If
*PROPERTY-RESOLVER* is NIL, then the parser will simply treat \\p and
\\P as #\\p and #\\P as in older versions of CL-PPCRE.")

(defvar *allow-quoting* nil
  "Whether the parser should support Perl's \\Q and \\E.")

(defvar *allow-named-registers* nil
  "Whether the parser should support AllegroCL's named registers
\(?<name>\"<regex>\") and back-reference \\k<name> syntax.")

(pushnew :cl-ppcre *features*)

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>
;; also used by LW-ADD-ONS

(defvar *hyperdoc-base-uri* "http://weitz.de/cl-ppcre/")

(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :cl-ppcre
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))
               
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/util.lisp,v 1.48 2009/10/28 07:36:15 edi Exp $

;;; Utility functions and constants dealing with the character sets we
;;; use to encode character classes

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'lw:with-unique-names))

#-:lispworks
(defmacro with-unique-names ((&rest bindings) &body body)
  "Syntax: WITH-UNIQUE-NAMES ( { var | (var x) }* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  `(let ,(mapcar #'(lambda (binding)
                     (check-type binding (or cons symbol))
                     (if (consp binding)
                       (destructuring-bind (var x) binding
                         (check-type var symbol)
                         `(,var (gensym ,(etypecase x
                                          (symbol (symbol-name x))
                                          (character (string x))
                                          (string x)))))
                       `(,binding (gensym ,(symbol-name binding)))))
                 bindings)
         ,@body))

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (macro-function 'with-rebinding)
          (macro-function 'lw:rebinding)))

#-:lispworks
(defmacro with-rebinding (bindings &body body)
  "WITH-REBINDING ( { var | (var prefix) }* ) form*

Evaluates a series of forms in the lexical environment that is
formed by adding the binding of each VAR to a fresh, uninterned
symbol, and the binding of that fresh, uninterned symbol to VAR's
original value, i.e., its value in the current lexical environment.

The uninterned symbol is created as if by a call to GENSYM with the
string denoted by PREFIX - or, if PREFIX is not supplied, the string
denoted by VAR - as argument.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3wv0fya0p.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  (loop for binding in bindings
        for var = (if (consp binding) (car binding) binding)
        for name = (gensym)
        collect `(,name ,var) into renames
        collect ``(,,var ,,name) into temps
        finally (return `(let ,renames
                          (with-unique-names ,bindings
                            `(let (,,@temps)
                              ,,@body))))))

(declaim (inline digit-char-p))  
(defun digit-char-p (chr)
  (declare #.*standard-optimize-settings*)
  "Tests whether a character is a decimal digit, i.e. the same as
Perl's [\\d].  Note that this function shadows the standard Common
Lisp function CL:DIGIT-CHAR-P."
  (char<= #\0 chr #\9))

(declaim (inline word-char-p))  
(defun word-char-p (chr)
  (declare #.*standard-optimize-settings*)
  "Tests whether a character is a \"word\" character.  In the ASCII
charset this is equivalent to a-z, A-Z, 0-9, or _, i.e. the same as
Perl's [\\w]."
  (or (alphanumericp chr)
      (char= chr #\_)))

(defconstant +whitespace-char-string+
  (coerce '(#\Space #\Tab #\Linefeed #\Return #\Page) 'string)
  "A string of all characters which are considered to be whitespace.
Same as Perl's [\\s].")

(defun whitespacep (chr)
  (declare #.*special-optimize-settings*)
  "Tests whether a character is whitespace, i.e. whether it would
match [\\s] in Perl."
  (find chr +whitespace-char-string+ :test #'char=))

(defmacro maybe-coerce-to-simple-string (string)
  "Coerces STRING to a simple STRING unless it already is one."
  (with-unique-names (=string=)
    `(let ((,=string= ,string))
      (cond (#+:lispworks
             (lw:simple-text-string-p ,=string=)
             #-:lispworks
             (simple-string-p ,=string=)
              ,=string=)
            (t
             (coerce ,=string=
                     #+:lispworks 'lw:simple-text-string
                     #-:lispworks 'simple-string))))))

(declaim (inline nsubseq))
(defun nsubseq (sequence start &optional (end (length sequence)))
  "Returns a subsequence by pointing to location in original sequence."
  (make-array (- end start)
              :element-type (array-element-type sequence)
              :displaced-to sequence
              :displaced-index-offset start))

(defun normalize-var-list (var-list)
  "Utility function for REGISTER-GROUPS-BIND and DO-REGISTER-GROUPS.
Creates the long form \(a list of \(FUNCTION VAR) entries) out of the
short form of VAR-LIST."
  (loop for element in var-list
        if (consp element)
          nconc (loop for var in (rest element)
                      collect (list (first element) var))
        else
          collect (list '(function identity) element)))

(defun string-list-to-simple-string (string-list)
  "Concatenates a list of strings to one simple-string."
  (declare #.*standard-optimize-settings*)
  ;; this function provided by JP Massar; note that we can't use APPLY
  ;; with CONCATENATE here because of CALL-ARGUMENTS-LIMIT
  (let ((total-size 0))
    (declare (fixnum total-size))
    (dolist (string string-list)
      #-:genera (declare (string string))
      (incf total-size (length string)))
    (let ((result-string (make-sequence #-:lispworks 'simple-string
                                        #+:lispworks 'lw:simple-text-string
                                        total-size))
          (curr-pos 0))
      (declare (fixnum curr-pos))
      (dolist (string string-list)
        #-:genera (declare (string string))
        (replace result-string string :start1 curr-pos)
        (incf curr-pos (length string)))
      result-string)))

(defun complement* (test-function)
  "Like COMPLEMENT but optimized for unary functions."
  (declare #.*standard-optimize-settings*)
  (typecase test-function
    (function
     (lambda (char)
       (declare (character char))
       (not (funcall (the function test-function) char))))
    (otherwise
     (lambda (char)
       (declare (character char))
       (not (funcall test-function char))))));;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/errors.lisp,v 1.22 2009/09/17 19:17:31 edi Exp $

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

(defvar *syntax-error-string* nil
  "The string which caused the syntax error.")

(define-condition ppcre-error (simple-error)
  ()
  (:documentation "All errors signaled by CL-PPCRE are of
this type."))

(define-condition ppcre-syntax-error (ppcre-error)
  ((string :initarg :string
           :reader ppcre-syntax-error-string)
   (pos :initarg :pos
        :reader ppcre-syntax-error-pos))
  (:default-initargs
      :pos nil
      :string *syntax-error-string*)
  (:report (lambda (condition stream)
             (format stream "~?~@[ at position ~A~]~@[ in string ~S~]"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)
                     (ppcre-syntax-error-pos condition)
                     (ppcre-syntax-error-string condition))))
  (:documentation "Signaled if CL-PPCRE's parser encounters an error
when trying to parse a regex string or to convert a parse tree into
its internal representation."))

(setf (documentation 'ppcre-syntax-error-string 'function)
      "Returns the string the parser was parsing when the error was
encountered \(or NIL if the error happened while trying to convert a
parse tree).")

(setf (documentation 'ppcre-syntax-error-pos 'function)
      "Returns the position within the string where the error occurred
\(or NIL if the error happened while trying to convert a parse tree")

(define-condition ppcre-invocation-error (ppcre-error)
  ()
  (:documentation "Signaled when CL-PPCRE functions are
invoked with wrong arguments."))

(defmacro signal-syntax-error* (pos format-control &rest format-arguments)
  `(error 'ppcre-syntax-error
          :pos ,pos
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))

(defmacro signal-syntax-error (format-control &rest format-arguments)
  `(signal-syntax-error* nil ,format-control ,@format-arguments))

(defmacro signal-invocation-error (format-control &rest format-arguments)
  `(error 'ppcre-invocation-error
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/charset.lisp,v 1.10 2009/09/17 19:17:30 edi Exp $

;;; A specialized set implementation for characters by Nikodemus Siivola.

;;; Copyright (c) 2008, Nikodemus Siivola. All rights reserved.
;;; Copyright (c) 2008-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

(defconstant +probe-depth+ 3
  "Maximum number of collisions \(for any element) we accept before we
allocate more storage.  This is now fixed, but could be made to vary
depending on the size of the storage vector \(e.g. in the range of
1-4).  Larger probe-depths mean more collisions are tolerated before
the table grows, but increase the constant factor.")

(defun make-char-vector (size)
  "Returns a vector of size SIZE to hold characters.  All elements are
initialized to #\Null except for the first one which is initialized to
#\?."
  (declare #.*standard-optimize-settings*)
  (declare (type (integer 2 #.(1- array-total-size-limit)) size))
  ;; since #\Null always hashes to 0, store something else there
  ;; initially, and #\Null everywhere else
  (let ((result (make-array size
                            :element-type #-:lispworks 'character #+:lispworks 'lw:simple-char
                            :initial-element (code-char 0))))
    (setf (char result 0) #\?)
    result))

(defstruct (charset (:constructor make-charset ()))
  ;; this is set to 0 when we stop hashing and just use a CHAR-CODE
  ;; indexed vector
  (depth +probe-depth+ :type fixnum)
  ;; the number of characters in this set
  (count 0 :type fixnum)
  ;; the storage vector
  (vector (make-char-vector 12) :type (simple-array character (*))))

;; seems to be necessary for some Lisps like ClozureCL
(defmethod make-load-form ((set charset) &optional environment)
  (make-load-form-saving-slots set :environment environment))

(declaim (inline mix))
(defun mix (code hash)
  "Given a character code CODE and a hash code HASH, computes and
returns the \"next\" hash code.  See comments below."
  (declare #.*standard-optimize-settings*)
  ;; mixing the CHAR-CODE back in at each step makes sure that if two
  ;; characters collide (their hashes end up pointing in the same
  ;; storage vector index) on one round, they should (hopefully!) not
  ;; collide on the next
  (sxhash (logand most-positive-fixnum (+ code hash))))

(declaim (inline compute-index))
(defun compute-index (hash vector)
  "Computes and returns the index into the vector VECTOR corresponding
to the hash code HASH."
  (declare #.*standard-optimize-settings*)
  (1+ (mod hash (1- (length vector)))))

(defun in-charset-p (char set)
  "Checks whether the character CHAR is in the charset SET."
  (declare #.*standard-optimize-settings*)
  (declare (character char) (charset set))
  (let ((vector (charset-vector set))
        (depth (charset-depth set))
        (code (char-code char)))
    (declare (fixnum depth))
    ;; as long as the set remains reasonably small, we use non-linear
    ;; hashing - the first hash of any character is its CHAR-CODE, and
    ;; subsequent hashes are computed by MIX above
    (cond ((or
            ;; depth 0 is special - each char maps only to its code,
            ;; nothing else
            (zerop depth)
            ;; index 0 is special - only #\Null maps to it, no matter
            ;; what the depth is
            (zerop code))
           (eq char (char vector code)))
          (t
           ;; otherwise hash starts out as the character code, but
           ;; maps to indexes 1-N
           (let ((hash code))
             (tagbody
              :retry
              (let* ((index (compute-index hash vector))
                     (x (char vector index)))
                (cond ((eq x (code-char 0))
                       ;; empty, no need to probe further
                       (return-from in-charset-p nil))
                      ((eq x char)
                       ;; got it
                       (return-from in-charset-p t))
                      ((zerop (decf depth))
                       ;; max probe depth reached, nothing found
                       (return-from in-charset-p nil))
                      (t
                       ;; nothing yet, try next place
                       (setf hash (mix code hash))
                       (go :retry))))))))))

(defun add-to-charset (char set)
  "Adds the character CHAR to the charset SET, extending SET if
necessary.  Returns CHAR."
  (declare #.*standard-optimize-settings*)
  (or (%add-to-charset char set t)
      (%add-to-charset/expand char set)
      (error "Oops, this should not happen..."))
  char)

(defun %add-to-charset (char set count)
  "Tries to add the character CHAR to the charset SET without
extending it.  Returns NIL if this fails.  Counts CHAR as new
if COUNT is true and it is added to SET."
  (declare #.*standard-optimize-settings*)
  (declare (character char) (charset set))
  (let ((vector (charset-vector set))
        (depth (charset-depth set))
        (code (char-code char)))
    (declare (fixnum depth))
    ;; see comments in IN-CHARSET-P for algorithm
    (cond ((or (zerop depth) (zerop code))
           (unless (eq char (char vector code))
             (setf (char vector code) char)
             (when count
               (incf (charset-count set))))
           char)
          (t
           (let ((hash code))
             (tagbody
              :retry
              (let* ((index (compute-index hash vector))
                     (x (char vector index)))
                (cond ((eq x (code-char 0))
                       (setf (char vector index) char)
                       (when count
                         (incf (charset-count set)))
                       (return-from %add-to-charset char))
                      ((eq x char)
                       (return-from %add-to-charset char))
                      ((zerop (decf depth))
                       ;; need to expand the table
                       (return-from %add-to-charset nil))
                      (t
                       (setf hash (mix code hash))
                       (go :retry))))))))))

(defun %add-to-charset/expand (char set)
  "Extends the charset SET and then adds the character CHAR to it."
  (declare #.*standard-optimize-settings*)
  (declare (character char) (charset set))
  (let* ((old-vector (charset-vector set))
         (new-size (* 2 (length old-vector))))
    (tagbody
     :retry
     ;; when the table grows large (currently over 1/3 of
     ;; CHAR-CODE-LIMIT), we dispense with hashing and just allocate a
     ;; storage vector with space for all characters, so that each
     ;; character always uses only the CHAR-CODE
     (multiple-value-bind (new-depth new-vector)
         (if (>= new-size #.(truncate char-code-limit 3))
           (values 0 (make-char-vector char-code-limit))
           (values +probe-depth+ (make-char-vector new-size)))
       (setf (charset-depth set) new-depth
             (charset-vector set) new-vector)
       (flet ((try-add (x)
                ;; don't count - old characters are already accounted
                ;; for, and might count the new one multiple times as
                ;; well
                (unless (%add-to-charset x set nil)
                  (assert (not (zerop new-depth)))
                  (setf new-size (* 2 new-size))
                  (go :retry))))
         (try-add char)
         (dotimes (i (length old-vector))
           (let ((x (char old-vector i)))
             (if (eq x (code-char 0))
               (when (zerop i)
                 (try-add x))
               (unless (zerop i)
                 (try-add x))))))))
    ;; added and expanded, /now/ count the new character.
    (incf (charset-count set))
    t))

(defun map-charset (function charset)
  "Calls FUNCTION with all characters in SET.  Returns NIL."
  (declare #.*standard-optimize-settings*)
  (declare (function function))
  (let* ((n (charset-count charset))
         (vector (charset-vector charset))
         (size (length vector)))
    ;; see comments in IN-CHARSET-P for algorithm
    (when (eq (code-char 0) (char vector 0))
      (funcall function (code-char 0))
      (decf n))
    (loop for i from 1 below size
          for char = (char vector i)
          unless (eq (code-char 0) char) do
          (funcall function char)
          ;; this early termination test should be worth it when
          ;; mapping across depth 0 charsets.
          (when (zerop (decf n))
            (return-from map-charset nil))))
  nil)

(defun create-charset-from-test-function (test-function start end)
  "Creates and returns a charset representing all characters with
character codes between START and END which satisfy TEST-FUNCTION."
  (declare #.*standard-optimize-settings*)
  (loop with charset = (make-charset)
        for code from start below end
        for char = (code-char code)
        when (and char (funcall test-function char))
        do (add-to-charset char charset)
        finally (return charset)))
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/charmap.lisp,v 1.19 2009/09/17 19:17:30 edi Exp $

;;; An optimized representation of sets of characters.

;;; Copyright (c) 2008-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

(defstruct (charmap  (:constructor make-charmap%))
  ;; a bit vector mapping char codes to "booleans" (1 for set members,
  ;; 0 for others)
  (vector #*0 :type simple-bit-vector)
  ;; the smallest character code of all characters in the set
  (start 0 :type fixnum)
  ;; the upper (exclusive) bound of all character codes in the set
  (end 0 :type fixnum)
  ;; the number of characters in the set, or NIL if this is unknown
  (count nil :type (or fixnum null))
  ;; whether the charmap actually represents the complement of the set  
  (complementp nil :type boolean))

;; seems to be necessary for some Lisps like ClozureCL
(defmethod make-load-form ((map charmap) &optional environment)
  (make-load-form-saving-slots map :environment environment))

(declaim (inline in-charmap-p))
(defun in-charmap-p (char charmap)
  "Tests whether the character CHAR belongs to the set represented by CHARMAP."
  (declare #.*standard-optimize-settings*)
  (declare (character char) (charmap charmap))
  (let* ((char-code (char-code char))
         (char-in-vector-p
          (let ((charmap-start (charmap-start charmap)))
            (declare (fixnum charmap-start))
            (and (<= charmap-start char-code)
                 (< char-code (the fixnum (charmap-end charmap)))
                 (= 1 (sbit (the simple-bit-vector (charmap-vector charmap))
                            (- char-code charmap-start)))))))
    (cond ((charmap-complementp charmap) (not char-in-vector-p))
          (t char-in-vector-p))))

(defun charmap-contents (charmap)
  "Returns a list of all characters belonging to a character map.
Only works for non-complement charmaps."
  (declare #.*standard-optimize-settings*)
  (declare (charmap charmap))
  (and (not (charmap-complementp charmap))
       (loop for code of-type fixnum from (charmap-start charmap) to (charmap-end charmap)
             for i across (the simple-bit-vector (charmap-vector charmap))
             when (= i 1)
             collect (code-char code))))

(defun make-charmap (start end test-function &optional complementp)
  "Creates and returns a charmap representing all characters with
character codes in the interval [start end) that satisfy
TEST-FUNCTION.  The COMPLEMENTP slot of the charmap is set to the
value of the optional argument, but this argument doesn't have an
effect on how TEST-FUNCTION is used."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum start end))
  (let ((vector (make-array (- end start) :element-type 'bit))
        (count 0))
    (declare (fixnum count))
    (loop for code from start below end
          for char = (code-char code)
          for index from 0
          when char do
          (incf count)
          (setf (sbit vector index) (if (funcall test-function char) 1 0)))
    (make-charmap% :vector vector
                   :start start
                   :end end
                   ;; we don't know for sure if COMPLEMENTP is true as
                   ;; there isn't a necessary a character for each
                   ;; integer below *REGEX-CHAR-CODE-LIMIT*
                   :count (and (not complementp) count)
                   ;; make sure it's boolean
                   :complementp (not (not complementp)))))

(defun create-charmap-from-test-function (test-function start end)
  "Creates and returns a charmap representing all characters with
character codes between START and END which satisfy TEST-FUNCTION.
Tries to find the smallest interval which is necessary to represent
the character set and uses the complement representation if that
helps."
  (declare #.*standard-optimize-settings*)
  (let (start-in end-in start-out end-out)
    ;; determine the smallest intervals containing the set and its
    ;; complement, [start-in, end-in) and [start-out, end-out) - first
    ;; the lower bound
    (loop for code from start below end
          for char = (code-char code)
          until (and start-in start-out)
          when (and char
                    (not start-in)
                    (funcall test-function char))
          do (setq start-in code)
          when (and char
                    (not start-out)
                    (not (funcall test-function char)))
          do (setq start-out code))
    (unless start-in
      ;; no character satisfied the test, so return a "pseudo" charmap
      ;; where IN-CHARMAP-P is always false
      (return-from create-charmap-from-test-function
        (make-charmap% :count 0)))
    (unless start-out
      ;; no character failed the test, so return a "pseudo" charmap
      ;; where IN-CHARMAP-P is always true
      (return-from create-charmap-from-test-function
        (make-charmap% :complementp t)))
    ;; now determine upper bound
    (loop for code from (1- end) downto start
          for char = (code-char code)
          until (and end-in end-out)
          when (and char
                    (not end-in)
                    (funcall test-function char))
          do (setq end-in (1+ code))
          when (and char
                    (not end-out)
                    (not (funcall test-function char)))
          do (setq end-out (1+ code)))
    ;; use the smaller interval
    (cond ((<= (- end-in start-in) (- end-out start-out))
           (make-charmap start-in end-in test-function))
          (t (make-charmap start-out end-out (complement* test-function) t)))))
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/chartest.lisp,v 1.5 2009/09/17 19:17:30 edi Exp $

;;; Copyright (c) 2008-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

(defun create-hash-table-from-test-function (test-function start end)
  "Creates and returns a hash table representing all characters with
character codes between START and END which satisfy TEST-FUNCTION."
  (declare #.*standard-optimize-settings*)
  (loop with hash-table = (make-hash-table)
        for code from start below end
        for char = (code-char code)
        when (and char (funcall test-function char))
        do (setf (gethash char hash-table) t)
        finally (return hash-table)))

(defun create-optimized-test-function (test-function &key
                                                     (start 0)
                                                     (end *regex-char-code-limit*)
                                                     (kind *optimize-char-classes*))
  "Given a unary test function which is applicable to characters
returns a function which yields the same boolean results for all
characters with character codes from START to \(excluding) END.  If
KIND is NIL, TEST-FUNCTION will simply be returned.  Otherwise, KIND
should be one of:

* :HASH-TABLE - builds a hash table representing all characters which
                satisfy the test and returns a closure which checks if
                a character is in that hash table

* :CHARSET - instead of a hash table uses a \"charset\" which is a
             data structure using non-linear hashing and optimized to
             represent \(sparse) sets of characters in a fast and
             space-efficient way \(contributed by Nikodemus Siivola)

* :CHARMAP - instead of a hash table uses a bit vector to represent
             the set of characters

You can also use :HASH-TABLE* or :CHARSET* which are like :HASH-TABLE
and :CHARSET but use the complement of the set if the set contains
more than half of all characters between START and END.  This saves
space but needs an additional pass across all characters to create the
data structure.  There is no corresponding :CHARMAP* kind as the bit
vectors are already created to cover the smallest possible interval
which contains either the set or its complement."
  (declare #.*standard-optimize-settings*)
  (ecase kind
    ((nil) test-function)
    (:charmap
     (let ((charmap (create-charmap-from-test-function test-function start end)))
       (lambda (char)
         (in-charmap-p char charmap))))
    ((:charset :charset*)
     (let ((charset (create-charset-from-test-function test-function start end)))
       (cond ((or (eq kind :charset)
                  (<= (charset-count charset) (ceiling (- end start) 2)))
              (lambda (char)
                (in-charset-p char charset)))
             (t (setq charset (create-charset-from-test-function (complement* test-function)
                                                                 start end))
                (lambda (char)
                  (not (in-charset-p char charset)))))))
    ((:hash-table :hash-table*)
     (let ((hash-table (create-hash-table-from-test-function test-function start end)))
       (cond ((or (eq kind :hash-table)
                  (<= (hash-table-count hash-table) (ceiling (- end start) 2)))
              (lambda (char)
                (gethash char hash-table)))
             (t (setq hash-table (create-hash-table-from-test-function (complement* test-function)
                                                                       start end))
                (lambda (char)
                  (not (gethash char hash-table)))))))))
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/lexer.lisp,v 1.35 2009/09/17 19:17:31 edi Exp $

;;; The lexer's responsibility is to convert the regex string into a
;;; sequence of tokens which are in turn consumed by the parser.
;;;
;;; The lexer is aware of Perl's 'extended mode' and it also 'knows'
;;; (with a little help from the parser) how many register groups it
;;; has opened so far.  (The latter is necessary for interpreting
;;; strings like "\\10" correctly.)

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

(declaim (inline map-char-to-special-class))
(defun map-char-to-special-char-class (chr)
  (declare #.*standard-optimize-settings*)
  "Maps escaped characters like \"\\d\" to the tokens which represent
their associated character classes."
  (case chr
    ((#\d)
      :digit-class)
    ((#\D)
      :non-digit-class)
    ((#\w)
      :word-char-class)
    ((#\W)
      :non-word-char-class)
    ((#\s)
      :whitespace-char-class)
    ((#\S)
      :non-whitespace-char-class)))

(declaim (inline make-lexer-internal))
(defstruct (lexer (:constructor make-lexer-internal))
  "LEXER structures are used to hold the regex string which is
currently lexed and to keep track of the lexer's state."
  (str "" :type string :read-only t)
  (len 0 :type fixnum :read-only t)
  (reg 0 :type fixnum)
  (pos 0 :type fixnum)
  (last-pos nil :type list))

(defun make-lexer (string)
  (declare #-:genera (string string))
  (make-lexer-internal :str (maybe-coerce-to-simple-string string)
                       :len (length string)))

(declaim (inline end-of-string-p))
(defun end-of-string-p (lexer)
  (declare #.*standard-optimize-settings*)
  "Tests whether we're at the end of the regex string."
  (<= (lexer-len lexer)
      (lexer-pos lexer)))

(declaim (inline looking-at-p))
(defun looking-at-p (lexer chr)
  (declare #.*standard-optimize-settings*)
  "Tests whether the next character the lexer would see is CHR.
Does not respect extended mode."
  (and (not (end-of-string-p lexer))
       (char= (schar (lexer-str lexer) (lexer-pos lexer))
              chr)))

(declaim (inline next-char-non-extended))
(defun next-char-non-extended (lexer)
  (declare #.*standard-optimize-settings*)
  "Returns the next character which is to be examined and updates the
POS slot. Does not respect extended mode."
  (cond ((end-of-string-p lexer) nil)
        (t (prog1
               (schar (lexer-str lexer) (lexer-pos lexer))
             (incf (lexer-pos lexer))))))

(defun next-char (lexer)
  (declare #.*standard-optimize-settings*)
  "Returns the next character which is to be examined and updates the
POS slot. Respects extended mode, i.e.  whitespace, comments, and also
nested comments are skipped if applicable."
  (let ((next-char (next-char-non-extended lexer))
        last-loop-pos)
    (loop
      ;; remember where we started
      (setq last-loop-pos (lexer-pos lexer))
      ;; first we look for nested comments like (?#foo)
      (when (and next-char
                 (char= next-char #\()
                 (looking-at-p lexer #\?))
        (incf (lexer-pos lexer))
        (cond ((looking-at-p lexer #\#)
                ;; must be a nested comment - so we have to search for
                ;; the closing parenthesis
                (let ((error-pos (- (lexer-pos lexer) 2)))
                  (unless
                      ;; loop 'til ')' or end of regex string and
                      ;; return NIL if ')' wasn't encountered
                      (loop for skip-char = next-char
                            then (next-char-non-extended lexer)
                            while (and skip-char
                                       (char/= skip-char #\)))
                            finally (return skip-char))
                    (signal-syntax-error* error-pos "Comment group not closed.")))
                (setq next-char (next-char-non-extended lexer)))
              (t
                ;; undo effect of previous INCF if we didn't see a #
                (decf (lexer-pos lexer)))))
      (when *extended-mode-p*
        ;; now - if we're in extended mode - we skip whitespace and
        ;; comments; repeat the following loop while we look at
        ;; whitespace or #\#
        (loop while (and next-char
                         (or (char= next-char #\#)
                             (whitespacep next-char)))
              do (setq next-char
                         (if (char= next-char #\#)
                           ;; if we saw a comment marker skip until
                           ;; we're behind #\Newline...
                           (loop for skip-char = next-char
                                 then (next-char-non-extended lexer)
                                 while (and skip-char
                                            (char/= skip-char #\Newline))
                                 finally (return (next-char-non-extended lexer)))
                           ;; ...otherwise (whitespace) skip until we
                           ;; see the next non-whitespace character
                           (loop for skip-char = next-char
                                 then (next-char-non-extended lexer)
                                 while (and skip-char
                                            (whitespacep skip-char))
                                 finally (return skip-char))))))
      ;; if the position has moved we have to repeat our tests
      ;; because of cases like /^a (?#xxx) (?#yyy) {3}c/x which
      ;; would be equivalent to /^a{3}c/ in Perl
      (unless (> (lexer-pos lexer) last-loop-pos)
        (return next-char)))))

(declaim (inline fail))
(defun fail (lexer)
  (declare #.*standard-optimize-settings*)
  "Moves (LEXER-POS LEXER) back to the last position stored in
\(LEXER-LAST-POS LEXER) and pops the LAST-POS stack."
  (unless (lexer-last-pos lexer)
    (signal-syntax-error "LAST-POS stack of LEXER ~A is empty." lexer))
  (setf (lexer-pos lexer) (pop (lexer-last-pos lexer)))
  nil)

(defun get-number (lexer &key (radix 10) max-length no-whitespace-p)
  (declare #.*standard-optimize-settings*)
  "Read and consume the number the lexer is currently looking at and
return it. Returns NIL if no number could be identified.
RADIX is used as in PARSE-INTEGER. If MAX-LENGTH is not NIL we'll read
at most the next MAX-LENGTH characters. If NO-WHITESPACE-P is not NIL
we don't tolerate whitespace in front of the number."
  (when (or (end-of-string-p lexer)
            (and no-whitespace-p
                 (whitespacep (schar (lexer-str lexer) (lexer-pos lexer)))))
    (return-from get-number nil))
  (multiple-value-bind (integer new-pos)
      (parse-integer (lexer-str lexer)
                     :start (lexer-pos lexer)
                     :end (if max-length
                            (let ((end-pos (+ (lexer-pos lexer)
                                              (the fixnum max-length)))
                                  (lexer-len (lexer-len lexer)))
                              (if (< end-pos lexer-len)
                                end-pos
                                lexer-len))
                            (lexer-len lexer))
                     :radix radix
                     :junk-allowed t)
    (cond ((and integer (>= (the fixnum integer) 0))
            (setf (lexer-pos lexer) new-pos)
            integer)
          (t nil))))

(declaim (inline try-number))
(defun try-number (lexer &key (radix 10) max-length no-whitespace-p)
  (declare #.*standard-optimize-settings*)
  "Like GET-NUMBER but won't consume anything if no number is seen."
  ;; remember current position
  (push (lexer-pos lexer) (lexer-last-pos lexer))
  (let ((number (get-number lexer
                            :radix radix
                            :max-length max-length
                            :no-whitespace-p no-whitespace-p)))
    (or number (fail lexer))))

(declaim (inline make-char-from-code))
(defun make-char-from-code (number error-pos)
  (declare #.*standard-optimize-settings*)
  "Create character from char-code NUMBER. NUMBER can be NIL
which is interpreted as 0. ERROR-POS is the position where
the corresponding number started within the regex string."
  ;; only look at rightmost eight bits in compliance with Perl
  (let ((code (logand #o377 (the fixnum (or number 0)))))
    (or (and (< code char-code-limit)
             (code-char code))
        (signal-syntax-error* error-pos "No character for hex-code ~X." number))))

(defun unescape-char (lexer)
  (declare #.*standard-optimize-settings*)
  "Convert the characters\(s) following a backslash into a token
which is returned. This function is to be called when the backslash
has already been consumed. Special character classes like \\W are
handled elsewhere."
  (when (end-of-string-p lexer)
    (signal-syntax-error "String ends with backslash."))
  (let ((chr (next-char-non-extended lexer)))
    (case chr
      ((#\E)
        ;; if \Q quoting is on this is ignored, otherwise it's just an
        ;; #\E
        (if *allow-quoting*
          :void
          #\E))
      ((#\c)
        ;; \cx means control-x in Perl
        (let ((next-char (next-char-non-extended lexer)))
          (unless next-char
            (signal-syntax-error* (lexer-pos lexer) "Character missing after '\\c' at position ~A."))
          (code-char (logxor #x40 (char-code (char-upcase next-char))))))
      ((#\x)
        ;; \x should be followed by a hexadecimal char code,
        ;; two digits or less
        (let* ((error-pos (lexer-pos lexer))
               (number (get-number lexer :radix 16 :max-length 2 :no-whitespace-p t)))
          ;; note that it is OK if \x is followed by zero digits
          (make-char-from-code number error-pos)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
        ;; \x should be followed by an octal char code,
        ;; three digits or less
        (let* ((error-pos (decf (lexer-pos lexer)))
               (number (get-number lexer :radix 8 :max-length 3)))
          (make-char-from-code number error-pos)))
      ;; the following five character names are 'semi-standard'
      ;; according to the CLHS but I'm not aware of any implementation
      ;; that doesn't implement them
      ((#\t)
        #\Tab)
      ((#\n)
        #\Newline)
      ((#\r)
        #\Return)
      ((#\f)
        #\Page)
      ((#\b)
        #\Backspace)
      ((#\a)
        (code-char 7))                  ; ASCII bell
      ((#\e)
        (code-char 27))                 ; ASCII escape
      (otherwise
        ;; all other characters aren't affected by a backslash
        chr))))

(defun read-char-property (lexer first-char)
  (declare #.*standard-optimize-settings*)
  (unless (eql (next-char-non-extended lexer) #\{)
    (signal-syntax-error* (lexer-pos lexer) "Expected left brace after \\~A." first-char))
  (let ((name (with-output-to-string (out nil :element-type
                                          #+:lispworks 'lw:simple-char #-:lispworks 'character)
                  (loop
                   (let ((char (or (next-char-non-extended lexer)
                                   (signal-syntax-error "Unexpected EOF after \\~A{." first-char))))
                     (when (char= char #\})
                       (return))
                     (write-char char out))))))
    (list (if (char= first-char #\p) :property :inverted-property)
          name)))

(defun collect-char-class (lexer)
  "Reads and consumes characters from regex string until a right
bracket is seen.  Assembles them into a list \(which is returned) of
characters, character ranges, like \(:RANGE #\\A #\\E) for a-e, and
tokens representing special character classes."
  (declare #.*standard-optimize-settings*)
  (let ((start-pos (lexer-pos lexer))         ; remember start for error message
        hyphen-seen
        last-char
        list)
    (flet ((handle-char (c)
             "Do the right thing with character C depending on whether
we're inside a range or not."
             (cond ((and hyphen-seen last-char)
                    (setf (car list) (list :range last-char c)
                          last-char nil))
                   (t
                    (push c list)
                    (setq last-char c)))
             (setq hyphen-seen nil)))
      (loop for first = t then nil
            for c = (next-char-non-extended lexer)
            ;; leave loop if at end of string
            while c
            do (cond
                ((char= c #\\)
                 ;; we've seen a backslash
                 (let ((next-char (next-char-non-extended lexer)))
                   (case next-char
                     ((#\d #\D #\w #\W #\s #\S)
                      ;; a special character class
                      (push (map-char-to-special-char-class next-char) list)
                      ;; if the last character was a hyphen
                      ;; just collect it literally
                      (when hyphen-seen
                        (push #\- list))
                      ;; if the next character is a hyphen do the same
                      (when (looking-at-p lexer #\-)
                        (push #\- list)
                        (incf (lexer-pos lexer)))
                      (setq hyphen-seen nil))
                     ((#\P #\p)
                      ;; maybe a character property
                      (cond ((null *property-resolver*)
                             (handle-char next-char))
                            (t
                             (push (read-char-property lexer next-char) list)
                             ;; if the last character was a hyphen
                             ;; just collect it literally
                             (when hyphen-seen
                               (push #\- list))
                             ;; if the next character is a hyphen do the same
                             (when (looking-at-p lexer #\-)
                               (push #\- list)
                               (incf (lexer-pos lexer)))
                             (setq hyphen-seen nil))))
                     ((#\E)
                      ;; if \Q quoting is on we ignore \E,
                      ;; otherwise it's just a plain #\E
                      (unless *allow-quoting*
                        (handle-char #\E)))
                     (otherwise
                      ;; otherwise unescape the following character(s)
                      (decf (lexer-pos lexer))
                      (handle-char (unescape-char lexer))))))
                (first
                 ;; the first character must not be a right bracket
                 ;; and isn't treated specially if it's a hyphen
                 (handle-char c))
                ((char= c #\])
                 ;; end of character class
                 ;; make sure we collect a pending hyphen
                 (when hyphen-seen
                   (setq hyphen-seen nil)
                   (handle-char #\-))
                 ;; reverse the list to preserve the order intended
                 ;; by the author of the regex string
                 (return-from collect-char-class (nreverse list)))
                ((and (char= c #\-)
                      last-char
                      (not hyphen-seen))
                 ;; if the last character was 'just a character'
                 ;; we expect to be in the middle of a range
                 (setq hyphen-seen t))
                ((char= c #\-)
                 ;; otherwise this is just an ordinary hyphen
                 (handle-char #\-))
                (t
                 ;; default case - just collect the character
                 (handle-char c))))
      ;; we can only exit the loop normally if we've reached the end
      ;; of the regex string without seeing a right bracket
      (signal-syntax-error* start-pos "Missing right bracket to close character class."))))

(defun maybe-parse-flags (lexer)
  (declare #.*standard-optimize-settings*)
  "Reads a sequence of modifiers \(including #\\- to reverse their
meaning) and returns a corresponding list of \"flag\" tokens.  The
\"x\" modifier is treated specially in that it dynamically modifies
the behaviour of the lexer itself via the special variable
*EXTENDED-MODE-P*."
  (prog1
    (loop with set = t
          for chr = (next-char-non-extended lexer)
          unless chr
            do (signal-syntax-error "Unexpected end of string.")
          while (find chr "-imsx" :test #'char=)
          ;; the first #\- will invert the meaning of all modifiers
          ;; following it
          if (char= chr #\-)
            do (setq set nil)
          else if (char= chr #\x)
            do (setq *extended-mode-p* set)
          else collect (if set
                         (case chr
                           ((#\i)
                             :case-insensitive-p)
                           ((#\m)
                             :multi-line-mode-p)
                           ((#\s)
                             :single-line-mode-p))
                         (case chr
                           ((#\i)
                             :case-sensitive-p)
                           ((#\m)
                             :not-multi-line-mode-p)
                           ((#\s)
                             :not-single-line-mode-p))))
    (decf (lexer-pos lexer))))

(defun get-quantifier (lexer)
  (declare #.*standard-optimize-settings*)
  "Returns a list of two values (min max) if what the lexer is looking
at can be interpreted as a quantifier. Otherwise returns NIL and
resets the lexer to its old position."
  ;; remember starting position for FAIL and UNGET-TOKEN functions
  (push (lexer-pos lexer) (lexer-last-pos lexer))
  (let ((next-char (next-char lexer)))
    (case next-char
      ((#\*)
        ;; * (Kleene star): match 0 or more times
        '(0 nil))
      ((#\+)
        ;; +: match 1 or more times
        '(1 nil))
      ((#\?)
        ;; ?: match 0 or 1 times
        '(0 1))
      ((#\{)
        ;; one of
        ;;   {n}:   match exactly n times
        ;;   {n,}:  match at least n times
        ;;   {n,m}: match at least n but not more than m times
        ;; note that anything not matching one of these patterns will
        ;; be interpreted literally - even whitespace isn't allowed
        (let ((num1 (get-number lexer :no-whitespace-p t)))
          (if num1
            (let ((next-char (next-char-non-extended lexer)))
              (case next-char
                ((#\,)
                  (let* ((num2 (get-number lexer :no-whitespace-p t))
                         (next-char (next-char-non-extended lexer)))
                    (case next-char
                      ((#\})
                        ;; this is the case {n,} (NUM2 is NIL) or {n,m}
                        (list num1 num2))
                      (otherwise
                        (fail lexer)))))
                ((#\})
                  ;; this is the case {n}
                  (list num1 num1))
                (otherwise
                  (fail lexer))))
            ;; no number following left curly brace, so we treat it
            ;; like a normal character
            (fail lexer))))
      ;; cannot be a quantifier
      (otherwise
        (fail lexer)))))

(defun parse-register-name-aux (lexer)
  "Reads and returns the name in a named register group.  It is
assumed that the starting #\< character has already been read.  The
closing #\> will also be consumed."
  ;; we have to look for an ending > character now
  (let ((end-name (position #\>
                            (lexer-str lexer)
                            :start (lexer-pos lexer)
                            :test #'char=)))
    (unless end-name
      ;; there has to be > somewhere, syntax error otherwise
      (signal-syntax-error* (1- (lexer-pos lexer)) "Opening #\< in named group has no closing #\>."))
    (let ((name (subseq (lexer-str lexer)
                        (lexer-pos lexer)
                        end-name)))
      (unless (every #'(lambda (char)
                         (or (alphanumericp char)
                             (char= #\- char)))
                     name)
        ;; register name can contain only alphanumeric characters or #\-
        (signal-syntax-error* (lexer-pos lexer) "Invalid character in named register group."))
      ;; advance lexer beyond "<name>" part
      (setf (lexer-pos lexer) (1+ end-name))
      name)))

(declaim (inline unget-token))
(defun unget-token (lexer)
  (declare #.*standard-optimize-settings*)
  "Moves the lexer back to the last position stored in the LAST-POS stack."
  (if (lexer-last-pos lexer)
    (setf (lexer-pos lexer)
            (pop (lexer-last-pos lexer)))
    (error "No token to unget \(this should not happen)")))

(defun get-token (lexer)
  (declare #.*standard-optimize-settings*)
  "Returns and consumes the next token from the regex string \(or NIL)."
  ;; remember starting position for UNGET-TOKEN function
  (push (lexer-pos lexer)
        (lexer-last-pos lexer))
  (let ((next-char (next-char lexer)))
    (cond (next-char
           (case next-char
             ;; the easy cases first - the following six characters
             ;; always have a special meaning and get translated
             ;; into tokens immediately
             ((#\))
              :close-paren)
             ((#\|)
              :vertical-bar)
             ((#\?)
              :question-mark)
             ((#\.)
              :everything)
             ((#\^)
              :start-anchor)
             ((#\$)
              :end-anchor)
             ((#\+ #\*)
              ;; quantifiers will always be consumend by
              ;; GET-QUANTIFIER, they must not appear here
              (signal-syntax-error* (1- (lexer-pos lexer)) "Quantifier '~A' not allowed." next-char))
             ((#\{)
              ;; left brace isn't a special character in it's own
              ;; right but we must check if what follows might
              ;; look like a quantifier
              (let ((this-pos (lexer-pos lexer))
                    (this-last-pos (lexer-last-pos lexer)))
                (unget-token lexer)
                (when (get-quantifier lexer)
                  (signal-syntax-error* (car this-last-pos)
                                        "Quantifier '~A' not allowed."
                                        (subseq (lexer-str lexer)
                                                (car this-last-pos)
                                                (lexer-pos lexer))))
                (setf (lexer-pos lexer) this-pos
                      (lexer-last-pos lexer) this-last-pos)
                next-char))
             ((#\[)
              ;; left bracket always starts a character class
              (cons  (cond ((looking-at-p lexer #\^)
                            (incf (lexer-pos lexer))
                            :inverted-char-class)
                           (t
                            :char-class))
                     (collect-char-class lexer)))
             ((#\\)
              ;; backslash might mean different things so we have
              ;; to peek one char ahead:
              (let ((next-char (next-char-non-extended lexer)))
                (case next-char
                  ((#\A)
                   :modeless-start-anchor)
                  ((#\Z)
                   :modeless-end-anchor)
                  ((#\z)
                   :modeless-end-anchor-no-newline)
                  ((#\b)
                   :word-boundary)
                  ((#\B)
                   :non-word-boundary)
                  ((#\k)
                   (cond ((and *allow-named-registers*
                               (looking-at-p lexer #\<))
                          ;; back-referencing a named register
                          (incf (lexer-pos lexer))
                          (list :back-reference
                                (parse-register-name-aux lexer)))
                         (t
                          ;; false alarm, just unescape \k
                          #\k)))
                  ((#\d #\D #\w #\W #\s #\S)
                   ;; these will be treated like character classes
                   (map-char-to-special-char-class next-char))
                  ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                   ;; uh, a digit...
                   (let* ((old-pos (decf (lexer-pos lexer)))
                          ;; ...so let's get the whole number first
                          (backref-number (get-number lexer)))
                     (declare (fixnum backref-number))
                     (cond ((and (> backref-number (lexer-reg lexer))
                                 (<= 10 backref-number))
                            ;; \10 and higher are treated as octal
                            ;; character codes if we haven't
                            ;; opened that much register groups
                            ;; yet
                            (setf (lexer-pos lexer) old-pos)
                            ;; re-read the number from the old
                            ;; position and convert it to its
                            ;; corresponding character
                            (make-char-from-code (get-number lexer :radix 8 :max-length 3)
                                                 old-pos))
                           (t
                            ;; otherwise this must refer to a
                            ;; backreference
                            (list :back-reference backref-number)))))
                  ((#\0)
                   ;; this always means an octal character code
                   ;; (at most three digits)
                   (let ((old-pos (decf (lexer-pos lexer))))
                     (make-char-from-code (get-number lexer :radix 8 :max-length 3)
                                          old-pos)))
                  ((#\P #\p)
                   ;; might be a named property
                   (cond (*property-resolver* (read-char-property lexer next-char))
                         (t next-char)))
                  (otherwise
                   ;; in all other cases just unescape the
                   ;; character
                   (decf (lexer-pos lexer))
                   (unescape-char lexer)))))
             ((#\()
              ;; an open parenthesis might mean different things
              ;; depending on what follows...
              (cond ((looking-at-p lexer #\?)
                     ;; this is the case '(?' (and probably more behind)
                     (incf (lexer-pos lexer))
                     ;; we have to check for modifiers first
                     ;; because a colon might follow
                     (let* ((flags (maybe-parse-flags lexer))
                            (next-char (next-char-non-extended lexer)))
                       ;; modifiers are only allowed if a colon
                       ;; or a closing parenthesis are following
                       (when (and flags
                                  (not (find next-char ":)" :test #'char=)))
                         (signal-syntax-error* (car (lexer-last-pos lexer))
                                               "Sequence '~A' not recognized."
                                               (subseq (lexer-str lexer)
                                                       (car (lexer-last-pos lexer))
                                                       (lexer-pos lexer))))
                       (case next-char
                         ((nil)
                          ;; syntax error
                          (signal-syntax-error "End of string following '(?'."))
                         ((#\))
                          ;; an empty group except for the flags
                          ;; (if there are any)
                          (or (and flags
                                   (cons :flags flags))
                              :void))
                         ((#\()
                          ;; branch
                          :open-paren-paren)
                         ((#\>)
                          ;; standalone
                          :open-paren-greater)
                         ((#\=)
                          ;; positive look-ahead
                          :open-paren-equal)
                         ((#\!)
                          ;; negative look-ahead
                          :open-paren-exclamation)
                         ((#\:)
                          ;; non-capturing group - return flags as
                          ;; second value
                          (values :open-paren-colon flags))
                         ((#\<)
                          ;; might be a look-behind assertion or a named group, so
                          ;; check next character
                          (let ((next-char (next-char-non-extended lexer)))
                            (if (alpha-char-p next-char)
                                (progn
                                  ;; we have encountered a named group
                                  ;; are we supporting register naming?
                                  (unless *allow-named-registers*
                                    (signal-syntax-error* (1- (lexer-pos lexer))
                                                          "Character '~A' may not follow '(?<'."
                                                          next-char))
                                  ;; put the letter back
                                  (decf (lexer-pos lexer))
                                  ;; named group
                                  :open-paren-less-letter)
                                (case next-char
                                  ((#\=)
                                   ;; positive look-behind
                                   :open-paren-less-equal)
                                  ((#\!)
                                   ;; negative look-behind
                                   :open-paren-less-exclamation)
                                  ((#\))
                                   ;; Perl allows "(?<)" and treats
                                   ;; it like a null string
                                   :void)
                                  ((nil)
                                   ;; syntax error
                                   (signal-syntax-error "End of string following '(?<'."))
                                  (t
                                   ;; also syntax error
                                   (signal-syntax-error* (1- (lexer-pos lexer))
                                                         "Character '~A' may not follow '(?<'."
                                                         next-char ))))))
                         (otherwise
                          (signal-syntax-error* (1- (lexer-pos lexer))
                                                "Character '~A' may not follow '(?'."
                                                next-char)))))
                    (t
                     ;; if next-char was not #\? (this is within
                     ;; the first COND), we've just seen an opening
                     ;; parenthesis and leave it like that
                     :open-paren)))
             (otherwise
              ;; all other characters are their own tokens
              next-char)))
          ;; we didn't get a character (this if the "else" branch from
          ;; the first IF), so we don't return a token but NIL
          (t
           (pop (lexer-last-pos lexer))
           nil))))

(declaim (inline start-of-subexpr-p))
(defun start-of-subexpr-p (lexer)
  (declare #.*standard-optimize-settings*)
  "Tests whether the next token can start a valid sub-expression, i.e.
a stand-alone regex."
  (let* ((pos (lexer-pos lexer))
         (next-char (next-char lexer)))
    (not (or (null next-char)
             (prog1
               (member (the character next-char)
                       '(#\) #\|)
                       :test #'char=)
               (setf (lexer-pos lexer) pos))))))
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/parser.lisp,v 1.31 2009/09/17 19:17:31 edi Exp $

;;; The parser will - with the help of the lexer - parse a regex
;;; string and convert it into a "parse tree" (see docs for details
;;; about the syntax of these trees).  Note that the lexer might
;;; return illegal parse trees.  It is assumed that the conversion
;;; process later on will track them down.

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

(defun group (lexer)
  "Parses and consumes a <group>.
The productions are: <group> -> \"\(\"<regex>\")\"
                                \"\(?:\"<regex>\")\"
                                \"\(?>\"<regex>\")\"
                                \"\(?<flags>:\"<regex>\")\"
                                \"\(?=\"<regex>\")\"
                                \"\(?!\"<regex>\")\"
                                \"\(?<=\"<regex>\")\"
                                \"\(?<!\"<regex>\")\"
                                \"\(?\(\"<num>\")\"<regex>\")\"
                                \"\(?\(\"<regex>\")\"<regex>\")\"
                                \"\(?<name>\"<regex>\")\" \(when *ALLOW-NAMED-REGISTERS* is T)
                                <legal-token>
where <flags> is parsed by the lexer function MAYBE-PARSE-FLAGS.
Will return <parse-tree> or \(<grouping-type> <parse-tree>) where
<grouping-type> is one of six keywords - see source for details."
  (declare #.*standard-optimize-settings*)
  (multiple-value-bind (open-token flags)
      (get-token lexer)
    (cond ((eq open-token :open-paren-paren)
            ;; special case for conditional regular expressions; note
            ;; that at this point we accept a couple of illegal
            ;; combinations which'll be sorted out later by the
            ;; converter
            (let* ((open-paren-pos (car (lexer-last-pos lexer)))
                   ;; check if what follows "(?(" is a number
                   (number (try-number lexer :no-whitespace-p t))
                   ;; make changes to extended-mode-p local
                   (*extended-mode-p* *extended-mode-p*))
              (declare (fixnum open-paren-pos))
              (cond (number
                      ;; condition is a number (i.e. refers to a
                      ;; back-reference)
                      (let* ((inner-close-token (get-token lexer))
                             (reg-expr (reg-expr lexer))
                             (close-token (get-token lexer)))
                        (unless (eq inner-close-token :close-paren)
                          (signal-syntax-error* (+ open-paren-pos 2)
                                                "Opening paren has no matching closing paren."))
                        (unless (eq close-token :close-paren)
                          (signal-syntax-error* open-paren-pos
                                                "Opening paren has no matching closing paren."))
                        (list :branch number reg-expr)))
                    (t
                      ;; condition must be a full regex (actually a
                      ;; look-behind or look-ahead); and here comes a
                      ;; terrible kludge: instead of being cleanly
                      ;; separated from the lexer, the parser pushes
                      ;; back the lexer by one position, thereby
                      ;; landing in the middle of the 'token' "(?(" -
                      ;; yuck!!
                      (decf (lexer-pos lexer))
                      (let* ((inner-reg-expr (group lexer))
                             (reg-expr (reg-expr lexer))
                             (close-token (get-token lexer)))
                        (unless (eq close-token :close-paren)
                          (signal-syntax-error* open-paren-pos
                                                "Opening paren has no matching closing paren."))
                        (list :branch inner-reg-expr reg-expr))))))
          ((member open-token '(:open-paren
                                :open-paren-colon
                                :open-paren-greater
                                :open-paren-equal
                                :open-paren-exclamation
                                :open-paren-less-equal
                                :open-paren-less-exclamation
                                :open-paren-less-letter)
                   :test #'eq)
            ;; make changes to extended-mode-p local
            (let ((*extended-mode-p* *extended-mode-p*))
              ;; we saw one of the six token representing opening
              ;; parentheses
              (let* ((open-paren-pos (car (lexer-last-pos lexer)))
                     (register-name (when (eq open-token :open-paren-less-letter)
                                      (parse-register-name-aux lexer)))
                     (reg-expr (reg-expr lexer))
                     (close-token (get-token lexer)))
                (when (or (eq open-token :open-paren)
                          (eq open-token :open-paren-less-letter))
                  ;; if this is the "("<regex>")" or "(?"<name>""<regex>")" production we have to
                  ;; increment the register counter of the lexer
                  (incf (lexer-reg lexer)))
                (unless (eq close-token :close-paren)
                  ;; the token following <regex> must be the closing
                  ;; parenthesis or this is a syntax error
                  (signal-syntax-error* open-paren-pos
                                        "Opening paren has no matching closing paren."))
                (if flags
                  ;; if the lexer has returned a list of flags this must
                  ;; have been the "(?:"<regex>")" production
                  (cons :group (nconc flags (list reg-expr)))
                  (if (eq open-token :open-paren-less-letter)
                      (list :named-register register-name
                            reg-expr)
                      (list (case open-token
                              ((:open-paren)
                               :register)
                              ((:open-paren-colon)
                               :group)
                              ((:open-paren-greater)
                               :standalone)
                              ((:open-paren-equal)
                               :positive-lookahead)
                              ((:open-paren-exclamation)
                               :negative-lookahead)
                              ((:open-paren-less-equal)
                               :positive-lookbehind)
                              ((:open-paren-less-exclamation)
                               :negative-lookbehind))
                            reg-expr))))))
          (t
           ;; this is the <legal-token> production; <legal-token> is
           ;; any token which passes START-OF-SUBEXPR-P (otherwise
           ;; parsing had already stopped in the SEQ method)
           open-token))))

(defun greedy-quant (lexer)
  "Parses and consumes a <greedy-quant>.
The productions are: <greedy-quant> -> <group> | <group><quantifier>
where <quantifier> is parsed by the lexer function GET-QUANTIFIER.
Will return <parse-tree> or (:GREEDY-REPETITION <min> <max> <parse-tree>)."
  (declare #.*standard-optimize-settings*)
  (let* ((group (group lexer))
         (token (get-quantifier lexer)))
    (if token
      ;; if GET-QUANTIFIER returned a non-NIL value it's the
      ;; two-element list (<min> <max>)
      (list :greedy-repetition (first token) (second token) group)
      group)))

(defun quant (lexer)
  "Parses and consumes a <quant>.
The productions are: <quant> -> <greedy-quant> | <greedy-quant>\"?\".
Will return the <parse-tree> returned by GREEDY-QUANT and optionally
change :GREEDY-REPETITION to :NON-GREEDY-REPETITION."
  (declare #.*standard-optimize-settings*)
  (let* ((greedy-quant (greedy-quant lexer))
         (pos (lexer-pos lexer))
         (next-char (next-char lexer)))
    (when next-char
      (if (char= next-char #\?)
        (setf (car greedy-quant) :non-greedy-repetition)
        (setf (lexer-pos lexer) pos)))
    greedy-quant))

(defun seq (lexer)
  "Parses and consumes a <seq>.
The productions are: <seq> -> <quant> | <quant><seq>.
Will return <parse-tree> or (:SEQUENCE <parse-tree> <parse-tree>)."
  (declare #.*standard-optimize-settings*)
  (flet ((make-array-from-two-chars (char1 char2)
           (let ((string (make-array 2
                                     :element-type 'character
                                     :fill-pointer t
                                     :adjustable t)))
             (setf (aref string 0) char1)
             (setf (aref string 1) char2)
             string)))
    ;; Note that we're calling START-OF-SUBEXPR-P before we actually try
    ;; to parse a <seq> or <quant> in order to catch empty regular
    ;; expressions
    (if (start-of-subexpr-p lexer)
        (loop with seq-is-sequence-p = nil
              with last-cdr
              for quant = (quant lexer)
              for quant-is-char-p = (characterp quant)
              for seq = quant
              then
              (cond ((and quant-is-char-p (characterp seq))
                     (make-array-from-two-chars seq quant))
                    ((and quant-is-char-p (stringp seq))
                     (vector-push-extend quant seq)
                     seq)
                    ((not seq-is-sequence-p)
                     (setf last-cdr (list quant)
                           seq-is-sequence-p t)
                     (list* :sequence seq last-cdr))
                    ((and quant-is-char-p
                          (characterp (car last-cdr)))
                     (setf (car last-cdr)
                           (make-array-from-two-chars (car last-cdr)
                                                      quant))
                     seq)
                    ((and quant-is-char-p
                          (stringp (car last-cdr)))
                     (vector-push-extend quant (car last-cdr))
                     seq)
                    (t
                     ;; if <seq> is also a :SEQUENCE parse tree we merge
                     ;; both lists into one
                     (let ((cons (list quant)))
                       (psetf last-cdr cons
                              (cdr last-cdr) cons))
                     seq))
              while (start-of-subexpr-p lexer)
              finally (return seq))
        :void)))

(defun reg-expr (lexer)
  "Parses and consumes a <regex>, a complete regular expression.
The productions are: <regex> -> <seq> | <seq>\"|\"<regex>.
Will return <parse-tree> or (:ALTERNATION <parse-tree> <parse-tree>)."
  (declare #.*standard-optimize-settings*)
  (let ((pos (lexer-pos lexer)))
    (case (next-char lexer)
      ((nil)
        ;; if we didn't get any token we return :VOID which stands for
        ;; "empty regular expression"
        :void)
      ((#\|)
        ;; now check whether the expression started with a vertical
        ;; bar, i.e. <seq> - the left alternation - is empty
        (list :alternation :void (reg-expr lexer)))
      (otherwise
        ;; otherwise un-read the character we just saw and parse a
        ;; <seq> plus the character following it
        (setf (lexer-pos lexer) pos)
        (let* ((seq (seq lexer))
               (pos (lexer-pos lexer)))
          (case (next-char lexer)
            ((nil)
              ;; no further character, just a <seq>
              seq)
            ((#\|)
              ;; if the character was a vertical bar, this is an
              ;; alternation and we have the second production
              (let ((reg-expr (reg-expr lexer)))
                (cond ((and (consp reg-expr)
                            (eq (first reg-expr) :alternation))
                        ;; again we try to merge as above in SEQ
                        (setf (cdr reg-expr)
                                (cons seq (cdr reg-expr)))
                        reg-expr)
                      (t (list :alternation seq reg-expr)))))
            (otherwise
              ;; a character which is not a vertical bar - this is
              ;; either a syntax error or we're inside of a group and
              ;; the next character is a closing parenthesis; so we
              ;; just un-read the character and let another function
              ;; take care of it
              (setf (lexer-pos lexer) pos)
              seq)))))))

(defun parse-string (string)
  "Translate the regex string STRING into a parse tree."
  (declare #.*standard-optimize-settings*)
  (let* ((lexer (make-lexer string))
         (parse-tree (reg-expr lexer)))
    ;; check whether we've consumed the whole regex string
    (if (end-of-string-p lexer)
      parse-tree
      (signal-syntax-error* (lexer-pos lexer) "Expected end of string."))))
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/regex-class.lisp,v 1.44 2009/10/28 07:36:15 edi Exp $

;;; This file defines the REGEX class.  REGEX objects are used to
;;; represent the (transformed) parse trees internally

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

(defclass regex ()
  ()
  (:documentation "The REGEX base class.  All other classes inherit
from this one."))

(defclass seq (regex)
  ((elements :initarg :elements
             :accessor elements
             :type cons
             :documentation "A list of REGEX objects."))
  (:documentation "SEQ objects represents sequences of regexes.
\(Like \"ab\" is the sequence of \"a\" and \"b\".)"))

(defclass alternation (regex)
  ((choices :initarg :choices
            :accessor choices
            :type cons
            :documentation "A list of REGEX objects"))
  (:documentation "ALTERNATION objects represent alternations of
regexes.  \(Like \"a|b\" ist the alternation of \"a\" or \"b\".)"))

(defclass lookahead (regex)
  ((regex :initarg :regex
          :accessor regex
          :documentation "The REGEX object we're checking.")
   (positivep :initarg :positivep
              :reader positivep
              :documentation "Whether this assertion is positive."))
  (:documentation "LOOKAHEAD objects represent look-ahead assertions."))

(defclass lookbehind (regex)
  ((regex :initarg :regex
          :accessor regex
          :documentation "The REGEX object we're checking.")
   (positivep :initarg :positivep
              :reader positivep
              :documentation "Whether this assertion is positive.")
   (len :initarg :len
        :accessor len
        :type fixnum
        :documentation "The \(fixed) length of the enclosed regex."))
  (:documentation "LOOKBEHIND objects represent look-behind assertions."))

(defclass repetition (regex)
  ((regex :initarg :regex
          :accessor regex
          :documentation "The REGEX that's repeated.")
   (greedyp :initarg :greedyp
            :reader greedyp
            :documentation "Whether the repetition is greedy.")
   (minimum :initarg :minimum
            :accessor minimum
            :type fixnum
            :documentation "The minimal number of repetitions.")
   (maximum :initarg :maximum
            :accessor maximum
            :documentation "The maximal number of repetitions.
Can be NIL for unbounded.")
   (min-len :initarg :min-len
            :reader min-len
            :documentation "The minimal length of the enclosed regex.")
   (len :initarg :len
        :reader len
        :documentation "The length of the enclosed regex.  NIL if
unknown.")
   (min-rest :initform 0
             :accessor min-rest
             :type fixnum
             :documentation "The minimal number of characters which
must appear after this repetition.")
   (contains-register-p :initarg :contains-register-p
                        :reader contains-register-p
                        :documentation "Whether the regex contains a
register."))
  (:documentation "REPETITION objects represent repetitions of regexes."))

(defmethod print-object ((repetition repetition) stream)
  (print-unreadable-object (repetition stream :type t :identity t)
    (princ (regex repetition) stream)))

(defclass register (regex)
  ((regex :initarg :regex
          :accessor regex
          :documentation "The inner regex.")
   (num :initarg :num
        :reader num
        :type fixnum
        :documentation "The number of this register, starting from 0.
This is the index into *REGS-START* and *REGS-END*.")
   (name :initarg :name
         :reader name
         :documentation "Name of this register or NIL."))
  (:documentation "REGISTER objects represent register groups."))

(defmethod print-object ((register register) stream)
  (print-unreadable-object (register stream :type t :identity t)
    (princ (regex register) stream)))

(defclass standalone (regex)
  ((regex :initarg :regex
          :accessor regex
          :documentation "The inner regex."))
  (:documentation "A standalone regular expression."))
  
(defclass back-reference (regex)
  ((num :initarg :num
        :accessor num
        :type fixnum
        :documentation "The number of the register this
reference refers to.")
   (name :initarg :name
         :accessor name
         :documentation "The name of the register this
reference refers to or NIL.")
   (case-insensitive-p :initarg :case-insensitive-p
                       :reader case-insensitive-p
                       :documentation "Whether we check
case-insensitively."))
  (:documentation "BACK-REFERENCE objects represent backreferences."))

(defclass char-class (regex)
  ((test-function :initarg :test-function
                  :reader test-function
                  :type (or function symbol nil)
                  :documentation "A unary function \(accepting a
character) which stands in for the character class and does the work
of checking whether a character belongs to the class."))
  (:documentation "CHAR-CLASS objects represent character classes."))

(defclass str (regex)
  ((str :initarg :str
        :accessor str
        :type string
        :documentation "The actual string.")
   (len :initform 0
        :accessor len
        :type fixnum
        :documentation "The length of the string.")
   (case-insensitive-p :initarg :case-insensitive-p
                       :reader case-insensitive-p
                       :documentation "If we match case-insensitively.")
   (offset :initform nil
           :accessor offset
           :documentation "Offset from the left of the whole
parse tree. The first regex has offset 0. NIL if unknown, i.e. behind
a variable-length regex.")
   (skip :initform nil
         :initarg :skip
         :accessor skip
         :documentation "If we can avoid testing for this
string because the SCAN function has done this already.")
   (start-of-end-string-p :initform nil
                          :accessor start-of-end-string-p
                          :documentation "If this is the unique
STR which starts END-STRING (a slot of MATCHER)."))
  (:documentation "STR objects represent string."))

(defmethod print-object ((str str) stream)
  (print-unreadable-object (str stream :type t :identity t)
    (princ (str str) stream)))

(defclass anchor (regex)
  ((startp :initarg :startp
           :reader startp
           :documentation "Whether this is a \"start anchor\".")
   (multi-line-p :initarg :multi-line-p
                 :initform nil
                 :reader multi-line-p
                 :documentation "Whether we're in multi-line mode,
i.e. whether each #\\Newline is surrounded by anchors.")
   (no-newline-p :initarg :no-newline-p
                 :initform nil
                 :reader no-newline-p
                 :documentation "Whether we ignore #\\Newline at the end."))
  (:documentation "ANCHOR objects represent anchors like \"^\" or \"$\"."))

(defclass everything (regex)
  ((single-line-p :initarg :single-line-p
                  :reader single-line-p
                  :documentation "Whether we're in single-line mode,
i.e. whether we also match #\\Newline."))
  (:documentation "EVERYTHING objects represent regexes matching
\"everything\", i.e. dots."))

(defclass word-boundary (regex)
  ((negatedp :initarg :negatedp
             :reader negatedp
             :documentation "Whether we mean the opposite,
i.e. no word-boundary."))
  (:documentation "WORD-BOUNDARY objects represent word-boundary assertions."))

(defclass branch (regex)
  ((test :initarg :test
         :accessor test
         :documentation "The test of this branch, one of
LOOKAHEAD, LOOKBEHIND, or a number.")
   (then-regex :initarg :then-regex
               :accessor then-regex
               :documentation "The regex that's to be matched if the
test succeeds.")
   (else-regex :initarg :else-regex
               :initform (make-instance 'void)
               :accessor else-regex
               :documentation "The regex that's to be matched if the
test fails."))
  (:documentation "BRANCH objects represent Perl's conditional regular
expressions."))
    
(defclass filter (regex)
  ((fn :initarg :fn
       :accessor fn
       :type (or function symbol)
       :documentation "The user-defined function.")
   (len :initarg :len
        :reader len
        :documentation "The fixed length of this filter or NIL."))
  (:documentation "FILTER objects represent arbitrary functions
defined by the user."))

(defclass void (regex)
  ()
  (:documentation "VOID objects represent empty regular expressions."))

(defmethod initialize-instance :after ((str str) &rest init-args)
  (declare #.*standard-optimize-settings*)
  (declare (ignore init-args))
  "Automatically computes the length of a STR after initialization."
  (let ((str-slot (slot-value str 'str)))
    (unless (typep str-slot
                   #-:lispworks 'simple-string
                   #+:lispworks 'lw:simple-text-string)
      (setf (slot-value str 'str)
            (coerce str-slot
                   #-:lispworks 'simple-string
                   #+:lispworks 'lw:simple-text-string))))
  (setf (len str) (length (str str))))

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/regex-class-util.lisp,v 1.9 2009/09/17 19:17:31 edi Exp $

;;; This file contains some utility methods for REGEX objects.

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

;;; The following four methods allow a VOID object to behave like a
;;; zero-length STR object (only readers needed)

(defmethod len ((void void))
  (declare #.*standard-optimize-settings*)
  0)

(defmethod str ((void void))
  (declare #.*standard-optimize-settings*)
  "")

(defmethod skip ((void void))
  (declare #.*standard-optimize-settings*)
  nil)

(defmethod start-of-end-string-p ((void void))
  (declare #.*standard-optimize-settings*)
  nil)

(defgeneric case-mode (regex old-case-mode)
  (declare #.*standard-optimize-settings*)
  (:documentation "Utility function used by the optimizer (see GATHER-STRINGS).
Returns a keyword denoting the case-(in)sensitivity of a STR or its
second argument if the STR has length 0. Returns NIL for REGEX objects
which are not of type STR."))

(defmethod case-mode ((str str) old-case-mode)
  (declare #.*standard-optimize-settings*)
  (cond ((zerop (len str))
          old-case-mode)
        ((case-insensitive-p str)
          :case-insensitive)
        (t
          :case-sensitive)))

(defmethod case-mode ((regex regex) old-case-mode)
  (declare #.*standard-optimize-settings*)
  (declare (ignore old-case-mode))
  nil)

(defgeneric copy-regex (regex)
  (declare #.*standard-optimize-settings*)
  (:documentation "Implements a deep copy of a REGEX object."))

(defmethod copy-regex ((anchor anchor))
  (declare #.*standard-optimize-settings*)
  (make-instance 'anchor
                 :startp (startp anchor)
                 :multi-line-p (multi-line-p anchor)
                 :no-newline-p (no-newline-p anchor)))

(defmethod copy-regex ((everything everything))
  (declare #.*standard-optimize-settings*)
  (make-instance 'everything
                 :single-line-p (single-line-p everything)))

(defmethod copy-regex ((word-boundary word-boundary))
  (declare #.*standard-optimize-settings*)
  (make-instance 'word-boundary
                 :negatedp (negatedp word-boundary)))

(defmethod copy-regex ((void void))
  (declare #.*standard-optimize-settings*)
  (make-instance 'void))

(defmethod copy-regex ((lookahead lookahead))
  (declare #.*standard-optimize-settings*)
  (make-instance 'lookahead
                 :regex (copy-regex (regex lookahead))
                 :positivep (positivep lookahead)))

(defmethod copy-regex ((seq seq))
  (declare #.*standard-optimize-settings*)
  (make-instance 'seq
                 :elements (mapcar #'copy-regex (elements seq))))

(defmethod copy-regex ((alternation alternation))
  (declare #.*standard-optimize-settings*)
  (make-instance 'alternation
                 :choices (mapcar #'copy-regex (choices alternation))))

(defmethod copy-regex ((branch branch))
  (declare #.*standard-optimize-settings*)
  (with-slots (test)
      branch
    (make-instance 'branch
                   :test (if (typep test 'regex)
                           (copy-regex test)
                           test)
                   :then-regex (copy-regex (then-regex branch))
                   :else-regex (copy-regex (else-regex branch)))))

(defmethod copy-regex ((lookbehind lookbehind))
  (declare #.*standard-optimize-settings*)
  (make-instance 'lookbehind
                 :regex (copy-regex (regex lookbehind))
                 :positivep (positivep lookbehind)
                 :len (len lookbehind)))

(defmethod copy-regex ((repetition repetition))
  (declare #.*standard-optimize-settings*)
  (make-instance 'repetition
                 :regex (copy-regex (regex repetition))
                 :greedyp (greedyp repetition)
                 :minimum (minimum repetition)
                 :maximum (maximum repetition)
                 :min-len (min-len repetition)
                 :len (len repetition)
                 :contains-register-p (contains-register-p repetition)))

(defmethod copy-regex ((register register))
  (declare #.*standard-optimize-settings*)
  (make-instance 'register
                 :regex (copy-regex (regex register))
                 :num (num register)
                 :name (name register)))

(defmethod copy-regex ((standalone standalone))
  (declare #.*standard-optimize-settings*)
  (make-instance 'standalone
                 :regex (copy-regex (regex standalone))))

(defmethod copy-regex ((back-reference back-reference))
  (declare #.*standard-optimize-settings*)
  (make-instance 'back-reference
                 :num (num back-reference)
                 :case-insensitive-p (case-insensitive-p back-reference)))

(defmethod copy-regex ((char-class char-class))
  (declare #.*standard-optimize-settings*)
  (make-instance 'char-class
                 :test-function (test-function char-class)))

(defmethod copy-regex ((str str))
  (declare #.*standard-optimize-settings*)
  (make-instance 'str
                 :str (str str)
                 :case-insensitive-p (case-insensitive-p str)))

(defmethod copy-regex ((filter filter))
  (declare #.*standard-optimize-settings*)
  (make-instance 'filter
                 :fn (fn filter)
                 :len (len filter)))

;;; Note that COPY-REGEX and REMOVE-REGISTERS could have easily been
;;; wrapped into one function. Maybe in the next release...

;;; Further note that this function is used by CONVERT to factor out
;;; complicated repetitions, i.e. cases like
;;;   (a)* -> (?:a*(a))?
;;; This won't work for, say,
;;;   ((a)|(b))* -> (?:(?:a|b)*((a)|(b)))?
;;; and therefore we stop REGISTER removal once we see an ALTERNATION.

(defgeneric remove-registers (regex)
  (declare #.*standard-optimize-settings*)
  (:documentation "Returns a deep copy of a REGEX (see COPY-REGEX) and
optionally removes embedded REGISTER objects if possible and if the
special variable REMOVE-REGISTERS-P is true."))

(defmethod remove-registers ((register register))
  (declare #.*standard-optimize-settings*)
  (declare (special remove-registers-p reg-seen))
  (cond (remove-registers-p
          (remove-registers (regex register)))
        (t
          ;; mark REG-SEEN as true so enclosing REPETITION objects
          ;; (see method below) know if they contain a register or not
          (setq reg-seen t)
          (copy-regex register))))

(defmethod remove-registers ((repetition repetition))
  (declare #.*standard-optimize-settings*)
  (let* (reg-seen
         (inner-regex (remove-registers (regex repetition))))
    ;; REMOVE-REGISTERS will set REG-SEEN (see method above) if
    ;; (REGEX REPETITION) contains a REGISTER
    (declare (special reg-seen))
    (make-instance 'repetition
                   :regex inner-regex
                   :greedyp (greedyp repetition)
                   :minimum (minimum repetition)
                   :maximum (maximum repetition)
                   :min-len (min-len repetition)
                   :len (len repetition)
                   :contains-register-p reg-seen)))

(defmethod remove-registers ((standalone standalone))
  (declare #.*standard-optimize-settings*)
  (make-instance 'standalone
                 :regex (remove-registers (regex standalone))))

(defmethod remove-registers ((lookahead lookahead))
  (declare #.*standard-optimize-settings*)
  (make-instance 'lookahead
                 :regex (remove-registers (regex lookahead))
                 :positivep (positivep lookahead)))

(defmethod remove-registers ((lookbehind lookbehind))
  (declare #.*standard-optimize-settings*)
  (make-instance 'lookbehind
                 :regex (remove-registers (regex lookbehind))
                 :positivep (positivep lookbehind)
                 :len (len lookbehind)))

(defmethod remove-registers ((branch branch))
  (declare #.*standard-optimize-settings*)
  (with-slots (test)
      branch
    (make-instance 'branch
                   :test (if (typep test 'regex)
                           (remove-registers test)
                           test)
                   :then-regex (remove-registers (then-regex branch))
                   :else-regex (remove-registers (else-regex branch)))))

(defmethod remove-registers ((alternation alternation))
  (declare #.*standard-optimize-settings*)
  (declare (special remove-registers-p))
  ;; an ALTERNATION, so we can't remove REGISTER objects further down
  (setq remove-registers-p nil)
  (copy-regex alternation))

(defmethod remove-registers ((regex regex))
  (declare #.*standard-optimize-settings*)
  (copy-regex regex))

(defmethod remove-registers ((seq seq))
  (declare #.*standard-optimize-settings*)
  (make-instance 'seq
                 :elements (mapcar #'remove-registers (elements seq))))

(defgeneric everythingp (regex)
  (declare #.*standard-optimize-settings*)
  (:documentation "Returns an EVERYTHING object if REGEX is equivalent
to this object, otherwise NIL.  So, \"(.){1}\" would return true
\(i.e. the object corresponding to \".\", for example."))

(defmethod everythingp ((seq seq))
  (declare #.*standard-optimize-settings*)
  ;; we might have degenerate cases like (:SEQUENCE :VOID ...)
  ;; due to the parsing process
  (let ((cleaned-elements (remove-if #'(lambda (element)
                                         (typep element 'void))
                                     (elements seq))))
    (and (= 1 (length cleaned-elements))
         (everythingp (first cleaned-elements)))))

(defmethod everythingp ((alternation alternation))
  (declare #.*standard-optimize-settings*)
  (with-slots (choices)
      alternation
    (and (= 1 (length choices))
         ;; this is unlikely to happen for human-generated regexes,
         ;; but machine-generated ones might look like this
         (everythingp (first choices)))))

(defmethod everythingp ((repetition repetition))
  (declare #.*standard-optimize-settings*)
  (with-slots (maximum minimum regex)
      repetition
    (and maximum
         (= 1 minimum maximum)
         ;; treat "<regex>{1,1}" like "<regex>"
         (everythingp regex))))

(defmethod everythingp ((register register))
  (declare #.*standard-optimize-settings*)
  (everythingp (regex register)))

(defmethod everythingp ((standalone standalone))
  (declare #.*standard-optimize-settings*)
  (everythingp (regex standalone)))

(defmethod everythingp ((everything everything))
  (declare #.*standard-optimize-settings*)
  everything)

(defmethod everythingp ((regex regex))
  (declare #.*standard-optimize-settings*)
  ;; the general case for ANCHOR, BACK-REFERENCE, BRANCH, CHAR-CLASS,
  ;; LOOKAHEAD, LOOKBEHIND, STR, VOID, FILTER, and WORD-BOUNDARY
  nil)

(defgeneric regex-length (regex)
  (declare #.*standard-optimize-settings*)
  (:documentation "Return the length of REGEX if it is fixed, NIL otherwise."))

(defmethod regex-length ((seq seq))
  (declare #.*standard-optimize-settings*)
  ;; simply add all inner lengths unless one of them is NIL
  (loop for sub-regex in (elements seq)
        for len = (regex-length sub-regex)
        if (not len) do (return nil)
        sum len))

(defmethod regex-length ((alternation alternation))
  (declare #.*standard-optimize-settings*)
  ;; only return a true value if all inner lengths are non-NIL and
  ;; mutually equal
  (loop for sub-regex in (choices alternation)
        for old-len = nil then len
        for len = (regex-length sub-regex)
        if (or (not len)
               (and old-len (/= len old-len))) do (return nil)
        finally (return len)))

(defmethod regex-length ((branch branch))
  (declare #.*standard-optimize-settings*)
  ;; only return a true value if both alternations have a length and
  ;; if they're equal
  (let ((then-length (regex-length (then-regex branch))))
    (and then-length
         (eql then-length (regex-length (else-regex branch)))
         then-length)))

(defmethod regex-length ((repetition repetition))
  (declare #.*standard-optimize-settings*)
  ;; we can only compute the length of a REPETITION object if the
  ;; number of repetitions is fixed; note that we don't call
  ;; REGEX-LENGTH for the inner regex, we assume that the LEN slot is
  ;; always set correctly
  (with-slots (len minimum maximum)
      repetition
    (if (and len
             (eql minimum maximum))
      (* minimum len)
      nil)))

(defmethod regex-length ((register register))
  (declare #.*standard-optimize-settings*)
  (regex-length (regex register)))

(defmethod regex-length ((standalone standalone))
  (declare #.*standard-optimize-settings*)
  (regex-length (regex standalone)))

(defmethod regex-length ((back-reference back-reference))
  (declare #.*standard-optimize-settings*)
  ;; with enough effort we could possibly do better here, but
  ;; currently we just give up and return NIL
  nil)
    
(defmethod regex-length ((char-class char-class))
  (declare #.*standard-optimize-settings*)
  1)

(defmethod regex-length ((everything everything))
  (declare #.*standard-optimize-settings*)
  1)

(defmethod regex-length ((str str))
  (declare #.*standard-optimize-settings*)
  (len str))

(defmethod regex-length ((filter filter))
  (declare #.*standard-optimize-settings*)
  (len filter))

(defmethod regex-length ((regex regex))
  (declare #.*standard-optimize-settings*)
  ;; the general case for ANCHOR, LOOKAHEAD, LOOKBEHIND, VOID, and
  ;; WORD-BOUNDARY (which all have zero-length)
  0)

(defgeneric regex-min-length (regex)
  (declare #.*standard-optimize-settings*)
  (:documentation "Returns the minimal length of REGEX."))

(defmethod regex-min-length ((seq seq))
  (declare #.*standard-optimize-settings*)
  ;; simply add all inner minimal lengths
  (loop for sub-regex in (elements seq)
        for len = (regex-min-length sub-regex)
        sum len))

(defmethod regex-min-length ((alternation alternation))
  (declare #.*standard-optimize-settings*)
  ;; minimal length of an alternation is the minimal length of the
  ;; "shortest" element
  (loop for sub-regex in (choices alternation)
        for len = (regex-min-length sub-regex)
        minimize len))

(defmethod regex-min-length ((branch branch))
  (declare #.*standard-optimize-settings*)
  ;; minimal length of both alternations
  (min (regex-min-length (then-regex branch))
       (regex-min-length (else-regex branch))))

(defmethod regex-min-length ((repetition repetition))
  (declare #.*standard-optimize-settings*)
  ;; obviously the product of the inner minimal length and the minimal
  ;; number of repetitions
  (* (minimum repetition) (min-len repetition)))
    
(defmethod regex-min-length ((register register))
  (declare #.*standard-optimize-settings*)
  (regex-min-length (regex register)))
    
(defmethod regex-min-length ((standalone standalone))
  (declare #.*standard-optimize-settings*)
  (regex-min-length (regex standalone)))
    
(defmethod regex-min-length ((char-class char-class))
  (declare #.*standard-optimize-settings*)
  1)

(defmethod regex-min-length ((everything everything))
  (declare #.*standard-optimize-settings*)
  1)

(defmethod regex-min-length ((str str))
  (declare #.*standard-optimize-settings*)
  (len str))
    
(defmethod regex-min-length ((filter filter))
  (declare #.*standard-optimize-settings*)
  (or (len filter)
      0))

(defmethod regex-min-length ((regex regex))
  (declare #.*standard-optimize-settings*)
  ;; the general case for ANCHOR, BACK-REFERENCE, LOOKAHEAD,
  ;; LOOKBEHIND, VOID, and WORD-BOUNDARY
  0)

(defgeneric compute-offsets (regex start-pos)
  (declare #.*standard-optimize-settings*)
  (:documentation "Returns the offset the following regex would have
relative to START-POS or NIL if we can't compute it. Sets the OFFSET
slot of REGEX to START-POS if REGEX is a STR. May also affect OFFSET
slots of STR objects further down the tree."))

;; note that we're actually only interested in the offset of
;; "top-level" STR objects (see ADVANCE-FN in the SCAN function) so we
;; can stop at variable-length alternations and don't need to descend
;; into repetitions

(defmethod compute-offsets ((seq seq) start-pos)
  (declare #.*standard-optimize-settings*)
  (loop for element in (elements seq)
        ;; advance offset argument for next call while looping through
        ;; the elements
        for pos = start-pos then curr-offset
        for curr-offset = (compute-offsets element pos)
        while curr-offset
        finally (return curr-offset)))

(defmethod compute-offsets ((alternation alternation) start-pos)
  (declare #.*standard-optimize-settings*)
  (loop for choice in (choices alternation)
        for old-offset = nil then curr-offset
        for curr-offset = (compute-offsets choice start-pos)
        ;; we stop immediately if two alternations don't result in the
        ;; same offset
        if (or (not curr-offset)
               (and old-offset (/= curr-offset old-offset)))
          do (return nil)
        finally (return curr-offset)))

(defmethod compute-offsets ((branch branch) start-pos)
  (declare #.*standard-optimize-settings*)
  ;; only return offset if both alternations have equal value
  (let ((then-offset (compute-offsets (then-regex branch) start-pos)))
    (and then-offset
         (eql then-offset (compute-offsets (else-regex branch) start-pos))
         then-offset)))

(defmethod compute-offsets ((repetition repetition) start-pos)
  (declare #.*standard-optimize-settings*)
  ;; no need to descend into the inner regex
  (with-slots (len minimum maximum)
      repetition
    (if (and len
             (eq minimum maximum))
      ;; fixed number of repetitions, so we know how to proceed
      (+ start-pos (* minimum len))
      ;; otherwise return NIL
      nil)))

(defmethod compute-offsets ((register register) start-pos)
  (declare #.*standard-optimize-settings*)
  (compute-offsets (regex register) start-pos))
    
(defmethod compute-offsets ((standalone standalone) start-pos)
  (declare #.*standard-optimize-settings*)
  (compute-offsets (regex standalone) start-pos))
    
(defmethod compute-offsets ((char-class char-class) start-pos)
  (declare #.*standard-optimize-settings*)
  (1+ start-pos))
    
(defmethod compute-offsets ((everything everything) start-pos)
  (declare #.*standard-optimize-settings*)
  (1+ start-pos))
    
(defmethod compute-offsets ((str str) start-pos)
  (declare #.*standard-optimize-settings*)
  (setf (offset str) start-pos)
  (+ start-pos (len str)))

(defmethod compute-offsets ((back-reference back-reference) start-pos)
  (declare #.*standard-optimize-settings*)
  ;; with enough effort we could possibly do better here, but
  ;; currently we just give up and return NIL
  (declare (ignore start-pos))
  nil)

(defmethod compute-offsets ((filter filter) start-pos)
  (declare #.*standard-optimize-settings*)
  (let ((len (len filter)))
    (if len
      (+ start-pos len)
      nil)))

(defmethod compute-offsets ((regex regex) start-pos)
  (declare #.*standard-optimize-settings*)
  ;; the general case for ANCHOR, LOOKAHEAD, LOOKBEHIND, VOID, and
  ;; WORD-BOUNDARY (which all have zero-length)
  start-pos)
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/convert.lisp,v 1.57 2009/09/17 19:17:31 edi Exp $

;;; Here the parse tree is converted into its internal representation
;;; using REGEX objects.  At the same time some optimizations are
;;; already applied.

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

;;; The flags that represent the "ism" modifiers are always kept
;;; together in a three-element list. We use the following macros to
;;; access individual elements.

(defmacro case-insensitive-mode-p (flags)
  "Accessor macro to extract the first flag out of a three-element flag list."
  `(first ,flags))

(defmacro multi-line-mode-p (flags)
  "Accessor macro to extract the second flag out of a three-element flag list."
  `(second ,flags))

(defmacro single-line-mode-p (flags)
  "Accessor macro to extract the third flag out of a three-element flag list."
  `(third ,flags))

(defun set-flag (token)
  "Reads a flag token and sets or unsets the corresponding entry in
the special FLAGS list."
  (declare #.*standard-optimize-settings*)
  (declare (special flags))
  (case token
    ((:case-insensitive-p)
      (setf (case-insensitive-mode-p flags) t))
    ((:case-sensitive-p)
      (setf (case-insensitive-mode-p flags) nil))
    ((:multi-line-mode-p)
      (setf (multi-line-mode-p flags) t))
    ((:not-multi-line-mode-p)
      (setf (multi-line-mode-p flags) nil))
    ((:single-line-mode-p)
      (setf (single-line-mode-p flags) t))
    ((:not-single-line-mode-p)
      (setf (single-line-mode-p flags) nil))
    (otherwise
      (signal-syntax-error "Unknown flag token ~A." token))))

(defgeneric resolve-property (property)
  (:documentation "Resolves PROPERTY to a unary character test
function.  PROPERTY can either be a function designator or it can be a
string which is resolved using *PROPERTY-RESOLVER*.")
  (:method ((property-name string))
   (funcall *property-resolver* property-name))
  (:method ((function-name symbol))
   function-name)
  (:method ((test-function function))
   test-function))

(defun convert-char-class-to-test-function (list invertedp case-insensitive-p)
  "Combines all items in LIST into test function and returns a
logical-OR combination of these functions.  Items can be single
characters, character ranges like \(:RANGE #\\A #\\E), or special
character classes like :DIGIT-CLASS.  Does the right thing with
respect to case-\(in)sensitivity as specified by the special variable
FLAGS."
  (declare #.*standard-optimize-settings*)
  (declare (special flags))
  (let ((test-functions
         (loop for item in list
               collect (cond ((characterp item)
                              ;; rebind so closure captures the right one
                              (let ((this-char item))
                                (lambda (char)
                                  (declare (character char this-char))
                                  (char= char this-char))))
                             ((symbolp item)
                              (case item
                                ((:digit-class) #'digit-char-p)
                                ((:non-digit-class) (complement* #'digit-char-p))
                                ((:whitespace-char-class) #'whitespacep)
                                ((:non-whitespace-char-class) (complement* #'whitespacep))
                                ((:word-char-class) #'word-char-p)
                                ((:non-word-char-class) (complement* #'word-char-p))
                                (otherwise
                                 (signal-syntax-error "Unknown symbol ~A in character class." item))))
                             ((and (consp item)
                                   (eq (first item) :property))
                              (resolve-property (second item)))
                             ((and (consp item)
                                   (eq (first item) :inverted-property))
                              (complement* (resolve-property (second item))))
                             ((and (consp item)
                                   (eq (first item) :range))
                              (let ((from (second item))
                                    (to (third item)))
                                (when (char> from to)
                                  (signal-syntax-error "Invalid range from ~S to ~S in char-class." from to))
                                (lambda (char)
                                  (declare (character char from to))
                                  (char<= from char to))))
                             (t (signal-syntax-error "Unknown item ~A in char-class list." item))))))
    (unless test-functions
      (signal-syntax-error "Empty character class."))
    (cond ((cdr test-functions)           
           (cond ((and invertedp case-insensitive-p)
                  (lambda (char)
                    (declare (character char))
                    (loop with both-case-p = (both-case-p char)
                          with char-down = (if both-case-p (char-downcase char) char)
                          with char-up = (if both-case-p (char-upcase char) nil)
                          for test-function in test-functions
                          never (or (funcall test-function char-down)
                                    (and char-up (funcall test-function char-up))))))
                 (case-insensitive-p
                  (lambda (char)
                    (declare (character char))
                    (loop with both-case-p = (both-case-p char)
                          with char-down = (if both-case-p (char-downcase char) char)
                          with char-up = (if both-case-p (char-upcase char) nil)
                          for test-function in test-functions
                          thereis (or (funcall test-function char-down)
                                      (and char-up (funcall test-function char-up))))))
                 (invertedp
                  (lambda (char)
                    (loop for test-function in test-functions
                          never (funcall test-function char))))
                 (t
                  (lambda (char)
                    (loop for test-function in test-functions
                          thereis (funcall test-function char))))))
          ;; there's only one test-function
          (t (let ((test-function (first test-functions)))
               (cond ((and invertedp case-insensitive-p)
                      (lambda (char)
                        (declare (character char))
                        (not (or (funcall test-function (char-downcase char))
                                 (and (both-case-p char)
                                      (funcall test-function (char-upcase char)))))))
                     (case-insensitive-p
                      (lambda (char)
                        (declare (character char))
                        (or (funcall test-function (char-downcase char))
                            (and (both-case-p char)
                                 (funcall test-function (char-upcase char))))))
                     (invertedp (complement* test-function))
                     (t test-function)))))))

(defun maybe-split-repetition (regex
                               greedyp
                               minimum
                               maximum
                               min-len
                               length
                               reg-seen)
  "Splits a REPETITION object into a constant and a varying part if
applicable, i.e. something like
  a{3,} -> a{3}a*
The arguments to this function correspond to the REPETITION slots of
the same name."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum minimum)
           (type (or fixnum null) maximum))
  ;; note the usage of COPY-REGEX here; we can't use the same REGEX
  ;; object in both REPETITIONS because they will have different
  ;; offsets
  (when maximum
    (when (zerop maximum)
      ;; trivial case: don't repeat at all
      (return-from maybe-split-repetition
        (make-instance 'void)))
    (when (= 1 minimum maximum)
      ;; another trivial case: "repeat" exactly once
      (return-from maybe-split-repetition
        regex)))
  ;; first set up the constant part of the repetition
  ;; maybe that's all we need
  (let ((constant-repetition (if (plusp minimum)
                               (make-instance 'repetition
                                              :regex (copy-regex regex)
                                              :greedyp greedyp
                                              :minimum minimum
                                              :maximum minimum
                                              :min-len min-len
                                              :len length
                                              :contains-register-p reg-seen)
                               ;; don't create garbage if minimum is 0
                               nil)))
    (when (and maximum
               (= maximum minimum))
      (return-from maybe-split-repetition
        ;; no varying part needed because min = max
        constant-repetition))
    ;; now construct the varying part
    (let ((varying-repetition
            (make-instance 'repetition
                           :regex regex
                           :greedyp greedyp
                           :minimum 0
                           :maximum (if maximum (- maximum minimum) nil)
                           :min-len min-len
                           :len length
                           :contains-register-p reg-seen)))
      (cond ((zerop minimum)
              ;; min = 0, no constant part needed
              varying-repetition)
            ((= 1 minimum)
              ;; min = 1, constant part needs no REPETITION wrapped around
              (make-instance 'seq
                             :elements (list (copy-regex regex)
                                             varying-repetition)))
            (t
              ;; general case
              (make-instance 'seq
                             :elements (list constant-repetition
                                             varying-repetition)))))))

;; During the conversion of the parse tree we keep track of the start
;; of the parse tree in the special variable STARTS-WITH which'll
;; either hold a STR object or an EVERYTHING object. The latter is the
;; case if the regex starts with ".*" which implicitly anchors the
;; regex at the start (perhaps modulo #\Newline).

(defun maybe-accumulate (str)
  "Accumulate STR into the special variable STARTS-WITH if
ACCUMULATE-START-P (also special) is true and STARTS-WITH is either
NIL or a STR object of the same case mode. Always returns NIL."
  (declare #.*standard-optimize-settings*)
  (declare (special accumulate-start-p starts-with))
  (declare (ftype (function (t) fixnum) len))
  (when accumulate-start-p
    (etypecase starts-with
      (str
        ;; STARTS-WITH already holds a STR, so we check if we can
        ;; concatenate
        (cond ((eq (case-insensitive-p starts-with)
                   (case-insensitive-p str))
                ;; we modify STARTS-WITH in place
                (setf (len starts-with)
                        (+ (len starts-with) (len str)))
                ;; note that we use SLOT-VALUE because the accessor
                ;; STR has a declared FTYPE which doesn't fit here
                (adjust-array (slot-value starts-with 'str)
                              (len starts-with)
                              :fill-pointer t)
                (setf (subseq (slot-value starts-with 'str)
                              (- (len starts-with) (len str)))
                        (str str)
                      ;; STR objects that are parts of STARTS-WITH
                      ;; always have their SKIP slot set to true
                      ;; because the SCAN function will take care of
                      ;; them, i.e. the matcher can ignore them
                      (skip str) t))
              (t (setq accumulate-start-p nil))))
      (null
        ;; STARTS-WITH is still empty, so we create a new STR object
        (setf starts-with
                (make-instance 'str
                               :str ""
                               :case-insensitive-p (case-insensitive-p str))
              ;; INITIALIZE-INSTANCE will coerce the STR to a simple
              ;; string, so we have to fill it afterwards
              (slot-value starts-with 'str)
                (make-array (len str)
                            :initial-contents (str str)
                            :element-type 'character
                            :fill-pointer t
                            :adjustable t)
              (len starts-with)
                (len str)
              ;; see remark about SKIP above
              (skip str) t))
      (everything
        ;; STARTS-WITH already holds an EVERYTHING object - we can't
        ;; concatenate
        (setq accumulate-start-p nil))))
  nil)

(declaim (inline convert-aux))
(defun convert-aux (parse-tree)
  "Converts the parse tree PARSE-TREE into a REGEX object and returns
it.  Will also

  - split and optimize repetitions,
  - accumulate strings or EVERYTHING objects into the special variable
    STARTS-WITH,
  - keep track of all registers seen in the special variable REG-NUM,
  - keep track of all named registers seen in the special variable REG-NAMES
  - keep track of the highest backreference seen in the special
    variable MAX-BACK-REF,
  - maintain and adher to the currently applicable modifiers in the special
    variable FLAGS, and
  - maybe even wash your car..."
  (declare #.*standard-optimize-settings*)
  (if (consp parse-tree)
    (convert-compound-parse-tree (first parse-tree) parse-tree)
    (convert-simple-parse-tree parse-tree)))

(defgeneric convert-compound-parse-tree (token parse-tree &key)
  (declare #.*standard-optimize-settings*)
  (:documentation "Helper function for CONVERT-AUX which converts
parse trees which are conses and dispatches on TOKEN which is the
first element of the parse tree.")
  (:method ((token t) (parse-tree t) &key)
   (signal-syntax-error "Unknown token ~A in parse-tree." token)))

(defmethod convert-compound-parse-tree ((token (eql :sequence)) parse-tree &key)
  "The case for parse trees like \(:SEQUENCE {<regex>}*)."
  (declare #.*standard-optimize-settings*)
  (cond ((cddr parse-tree)
         ;; this is essentially like
         ;; (MAPCAR 'CONVERT-AUX (REST PARSE-TREE))
         ;; but we don't cons a new list
         (loop for parse-tree-rest on (rest parse-tree)
               while parse-tree-rest
               do (setf (car parse-tree-rest)
                        (convert-aux (car parse-tree-rest))))
         (make-instance 'seq :elements (rest parse-tree)))
        (t (convert-aux (second parse-tree)))))

(defmethod convert-compound-parse-tree ((token (eql :group)) parse-tree &key)
  "The case for parse trees like \(:GROUP {<regex>}*).

This is a syntactical construct equivalent to :SEQUENCE intended to
keep the effect of modifiers local."
  (declare #.*standard-optimize-settings*)
  (declare (special flags))
  ;; make a local copy of FLAGS and shadow the global value while we
  ;; descend into the enclosed regexes
  (let ((flags (copy-list flags)))
    (declare (special flags))
    (cond ((cddr parse-tree)
           (loop for parse-tree-rest on (rest parse-tree)
                 while parse-tree-rest
                 do (setf (car parse-tree-rest)
                          (convert-aux (car parse-tree-rest))))
           (make-instance 'seq :elements (rest parse-tree)))
          (t (convert-aux (second parse-tree))))))

(defmethod convert-compound-parse-tree ((token (eql :alternation)) parse-tree &key)
  "The case for \(:ALTERNATION {<regex>}*)."
  (declare #.*standard-optimize-settings*)
  (declare (special accumulate-start-p))
  ;; we must stop accumulating objects into STARTS-WITH once we reach
  ;; an alternation
  (setq accumulate-start-p nil)
  (loop for parse-tree-rest on (rest parse-tree)
        while parse-tree-rest
        do (setf (car parse-tree-rest)
                 (convert-aux (car parse-tree-rest))))
  (make-instance 'alternation :choices (rest parse-tree)))

(defmethod convert-compound-parse-tree ((token (eql :branch)) parse-tree &key)
  "The case for \(:BRANCH <test> <regex>).

Here, <test> must be look-ahead, look-behind or number; if <regex> is
an alternation it must have one or two choices."
  (declare #.*standard-optimize-settings*)
  (declare (special accumulate-start-p))
  (setq accumulate-start-p nil)
  (let* ((test-candidate (second parse-tree))
         (test (cond ((numberp test-candidate)
                      (when (zerop (the fixnum test-candidate))
                        (signal-syntax-error "Register 0 doesn't exist: ~S." parse-tree))
                      (1- (the fixnum test-candidate)))
                     (t (convert-aux test-candidate))))
         (alternations (convert-aux (third parse-tree))))
    (when (and (not (numberp test))
               (not (typep test 'lookahead))
               (not (typep test 'lookbehind)))
      (signal-syntax-error "Branch test must be look-ahead, look-behind or number: ~S." parse-tree))
    (typecase alternations
      (alternation
       (case (length (choices alternations))
         ((0)
          (signal-syntax-error "No choices in branch: ~S." parse-tree))
         ((1)
          (make-instance 'branch
                         :test test
                         :then-regex (first
                                      (choices alternations))))
         ((2)
          (make-instance 'branch
                         :test test
                         :then-regex (first
                                      (choices alternations))
                         :else-regex (second
                                      (choices alternations))))
         (otherwise
          (signal-syntax-error "Too much choices in branch: ~S." parse-tree))))
      (t
       (make-instance 'branch
                      :test test
                      :then-regex alternations)))))

(defmethod convert-compound-parse-tree ((token (eql :positive-lookahead)) parse-tree &key)
  "The case for \(:POSITIVE-LOOKAHEAD <regex>)."
  (declare #.*standard-optimize-settings*)
  (declare (special flags accumulate-start-p))
  ;; keep the effect of modifiers local to the enclosed regex and stop
  ;; accumulating into STARTS-WITH
  (setq accumulate-start-p nil)
  (let ((flags (copy-list flags)))
    (declare (special flags))
    (make-instance 'lookahead
                   :regex (convert-aux (second parse-tree))
                   :positivep t)))

(defmethod convert-compound-parse-tree ((token (eql :negative-lookahead)) parse-tree &key)
  "The case for \(:NEGATIVE-LOOKAHEAD <regex>)."
  (declare #.*standard-optimize-settings*)
  ;; do the same as for positive look-aheads and just switch afterwards
  (let ((regex (convert-compound-parse-tree :positive-lookahead parse-tree)))
    (setf (slot-value regex 'positivep) nil)
    regex))

(defmethod convert-compound-parse-tree ((token (eql :positive-lookbehind)) parse-tree &key)
  "The case for \(:POSITIVE-LOOKBEHIND <regex>)."
  (declare #.*standard-optimize-settings*)
  (declare (special flags accumulate-start-p))
  ;; keep the effect of modifiers local to the enclosed regex and stop
  ;; accumulating into STARTS-WITH
  (setq accumulate-start-p nil)
  (let* ((flags (copy-list flags))
         (regex (convert-aux (second parse-tree)))
         (len (regex-length regex)))
    (declare (special flags))
    ;; lookbehind assertions must be of fixed length
    (unless len
      (signal-syntax-error "Variable length look-behind not implemented \(yet): ~S." parse-tree))
    (make-instance 'lookbehind
                   :regex regex
                   :positivep t
                   :len len)))

(defmethod convert-compound-parse-tree ((token (eql :negative-lookbehind)) parse-tree &key)
  "The case for \(:NEGATIVE-LOOKBEHIND <regex>)."
  (declare #.*standard-optimize-settings*)
  ;; do the same as for positive look-behinds and just switch afterwards
  (let ((regex (convert-compound-parse-tree :positive-lookbehind parse-tree)))
    (setf (slot-value regex 'positivep) nil)
    regex))

(defmethod convert-compound-parse-tree ((token (eql :greedy-repetition)) parse-tree &key (greedyp t))
  "The case for \(:GREEDY-REPETITION|:NON-GREEDY-REPETITION <min> <max> <regex>).

This function is also used for the non-greedy case in which case it is
called with GREEDYP set to NIL as you would expect."
  (declare #.*standard-optimize-settings*)
  (declare (special accumulate-start-p starts-with))
  ;; remember the value of ACCUMULATE-START-P upon entering
  (let ((local-accumulate-start-p accumulate-start-p))
    (let ((minimum (second parse-tree))
          (maximum (third parse-tree)))
      (declare (fixnum minimum))
      (declare (type (or null fixnum) maximum))
      (unless (and maximum
                   (= 1 minimum maximum))
        ;; set ACCUMULATE-START-P to NIL for the rest of
        ;; the conversion because we can't continue to
        ;; accumulate inside as well as after a proper
        ;; repetition
        (setq accumulate-start-p nil))
      (let* (reg-seen
             (regex (convert-aux (fourth parse-tree)))
             (min-len (regex-min-length regex))
             (length (regex-length regex)))
        ;; note that this declaration already applies to
        ;; the call to CONVERT-AUX above
        (declare (special reg-seen))
        (when (and local-accumulate-start-p
                   (not starts-with)
                   (zerop minimum)
                   (not maximum))
          ;; if this repetition is (equivalent to) ".*"
          ;; and if we're at the start of the regex we
          ;; remember it for ADVANCE-FN (see the SCAN
          ;; function)
          (setq starts-with (everythingp regex)))
        (if (or (not reg-seen)
                (not greedyp)
                (not length)
                (zerop length)
                (and maximum (= minimum maximum)))
          ;; the repetition doesn't enclose a register, or
          ;; it's not greedy, or we can't determine it's
          ;; (inner) length, or the length is zero, or the
          ;; number of repetitions is fixed; in all of
          ;; these cases we don't bother to optimize
          (maybe-split-repetition regex
                                  greedyp
                                  minimum
                                  maximum
                                  min-len
                                  length
                                  reg-seen)
          ;; otherwise we make a transformation that looks
          ;; roughly like one of
          ;;   <regex>* -> (?:<regex'>*<regex>)?
          ;;   <regex>+ -> <regex'>*<regex>
          ;; where the trick is that as much as possible
          ;; registers from <regex> are removed in
          ;; <regex'>
          (let* (reg-seen ; new instance for REMOVE-REGISTERS
                 (remove-registers-p t)
                 (inner-regex (remove-registers regex))
                 (inner-repetition
                  ;; this is the "<regex'>" part
                  (maybe-split-repetition inner-regex
                                          ;; always greedy
                                          t
                                          ;; reduce minimum by 1
                                          ;; unless it's already 0
                                          (if (zerop minimum)
                                            0
                                            (1- minimum))
                                          ;; reduce maximum by 1
                                          ;; unless it's NIL
                                          (and maximum
                                               (1- maximum))
                                          min-len
                                          length
                                          reg-seen))
                 (inner-seq
                  ;; this is the "<regex'>*<regex>" part
                  (make-instance 'seq
                                 :elements (list inner-repetition
                                                 regex))))
            ;; note that this declaration already applies
            ;; to the call to REMOVE-REGISTERS above
            (declare (special remove-registers-p reg-seen))
            ;; wrap INNER-SEQ with a greedy
            ;; {0,1}-repetition (i.e. "?") if necessary
            (if (plusp minimum)
              inner-seq
              (maybe-split-repetition inner-seq
                                      t
                                      0
                                      1
                                      min-len
                                      nil
                                      t))))))))

(defmethod convert-compound-parse-tree ((token (eql :non-greedy-repetition)) parse-tree &key)
  "The case for \(:NON-GREEDY-REPETITION <min> <max> <regex>)."
  (declare #.*standard-optimize-settings*)
  ;; just dispatch to the method above with GREEDYP explicitly set to NIL
  (convert-compound-parse-tree :greedy-repetition parse-tree :greedyp nil))

(defmethod convert-compound-parse-tree ((token (eql :register)) parse-tree &key name)
  "The case for \(:REGISTER <regex>).  Also used for named registers
when NAME is not NIL."
  (declare #.*standard-optimize-settings*)
  (declare (special flags reg-num reg-names))
  ;; keep the effect of modifiers local to the enclosed regex; also,
  ;; assign the current value of REG-NUM to the corresponding slot of
  ;; the REGISTER object and increase this counter afterwards; for
  ;; named register update REG-NAMES and set the corresponding name
  ;; slot of the REGISTER object too
  (let ((flags (copy-list flags))
        (stored-reg-num reg-num))
    (declare (special flags reg-seen named-reg-seen))
    (setq reg-seen t)
    (when name (setq named-reg-seen t))
    (incf (the fixnum reg-num))
    (push name reg-names)
    (make-instance 'register
                   :regex (convert-aux (if name (third parse-tree) (second parse-tree)))
                   :num stored-reg-num
                   :name name)))

(defmethod convert-compound-parse-tree ((token (eql :named-register)) parse-tree &key)
  "The case for \(:NAMED-REGISTER <regex>)."
  (declare #.*standard-optimize-settings*)
  ;; call the method above and use the :NAME keyword argument
  (convert-compound-parse-tree :register parse-tree :name (copy-seq (second parse-tree))))

(defmethod convert-compound-parse-tree ((token (eql :filter)) parse-tree &key)
  "The case for \(:FILTER <function> &optional <length>)."
  (declare #.*standard-optimize-settings*)
  (declare (special accumulate-start-p))
  ;; stop accumulating into STARTS-WITH
  (setq accumulate-start-p nil)
  (make-instance 'filter
                 :fn (second parse-tree)
                 :len (third parse-tree)))

(defmethod convert-compound-parse-tree ((token (eql :standalone)) parse-tree &key)
  "The case for \(:STANDALONE <regex>)."
  (declare #.*standard-optimize-settings*)
  (declare (special flags accumulate-start-p))
  ;; stop accumulating into STARTS-WITH
  (setq accumulate-start-p nil)
  ;; keep the effect of modifiers local to the enclosed regex
  (let ((flags (copy-list flags)))
    (declare (special flags))
    (make-instance 'standalone :regex (convert-aux (second parse-tree)))))

(defmethod convert-compound-parse-tree ((token (eql :back-reference)) parse-tree &key)
  "The case for \(:BACK-REFERENCE <number>|<name>)."
  (declare #.*standard-optimize-settings*)
  (declare (special flags accumulate-start-p reg-num reg-names max-back-ref))
  (let* ((backref-name (and (stringp (second parse-tree))
                            (second parse-tree)))
         (referred-regs
          (when backref-name
            ;; find which register corresponds to the given name
            ;; we have to deal with case where several registers share
            ;; the same name and collect their respective numbers
            (loop for name in reg-names
                  for reg-index from 0
                  when (string= name backref-name)
                  ;; NOTE: REG-NAMES stores register names in reversed
                  ;; order REG-NUM contains number of (any) registers
                  ;; seen so far; 1- will be done later
                  collect (- reg-num reg-index))))
         ;; store the register number for the simple case
         (backref-number (or (first referred-regs) (second parse-tree))))
    (declare (type (or fixnum null) backref-number))
    (when (or (not (typep backref-number 'fixnum))
              (<= backref-number 0))
      (signal-syntax-error "Illegal back-reference: ~S." parse-tree))
    ;; stop accumulating into STARTS-WITH and increase MAX-BACK-REF if
    ;; necessary
    (setq accumulate-start-p nil
          max-back-ref (max (the fixnum max-back-ref)
                            backref-number))
    (flet ((make-back-ref (backref-number)
             (make-instance 'back-reference
                            ;; we start counting from 0 internally
                            :num (1- backref-number)
                            :case-insensitive-p (case-insensitive-mode-p flags)
                            ;; backref-name is NIL or string, safe to copy
                            :name (copy-seq backref-name))))
      (cond
       ((cdr referred-regs)
        ;; several registers share the same name we will try to match
        ;; any of them, starting with the most recent first
        ;; alternation is used to accomplish matching
        (make-instance 'alternation
                       :choices (loop
                                 for reg-index in referred-regs
                                 collect (make-back-ref reg-index))))
       ;; simple case - backref corresponds to only one register
       (t
        (make-back-ref backref-number))))))

(defmethod convert-compound-parse-tree ((token (eql :regex)) parse-tree &key)
  "The case for \(:REGEX <string>)."
  (declare #.*standard-optimize-settings*)
  (convert-aux (parse-string (second parse-tree))))

(defmethod convert-compound-parse-tree ((token (eql :char-class)) parse-tree &key invertedp)
  "The case for \(:CHAR-CLASS {<item>}*) where item is one of

- a character,
- a character range: \(:RANGE <char1> <char2>), or
- a special char class symbol like :DIGIT-CHAR-CLASS.

Also used for inverted char classes when INVERTEDP is true."
  (declare #.*standard-optimize-settings*)
  (declare (special flags accumulate-start-p))
  (let ((test-function
         (create-optimized-test-function
          (convert-char-class-to-test-function (rest parse-tree)
                                               invertedp
                                               (case-insensitive-mode-p flags)))))
    (setq accumulate-start-p nil)
    (make-instance 'char-class :test-function test-function)))

(defmethod convert-compound-parse-tree ((token (eql :inverted-char-class)) parse-tree &key)
  "The case for \(:INVERTED-CHAR-CLASS {<item>}*)."
  (declare #.*standard-optimize-settings*)
  ;; just dispatch to the "real" method
  (convert-compound-parse-tree :char-class parse-tree :invertedp t))

(defmethod convert-compound-parse-tree ((token (eql :property)) parse-tree &key)
  "The case for \(:PROPERTY <name>) where <name> is a string."
  (declare #.*standard-optimize-settings*)
  (declare (special accumulate-start-p))
  (setq accumulate-start-p nil)
  (make-instance 'char-class :test-function (resolve-property (second parse-tree))))

(defmethod convert-compound-parse-tree ((token (eql :inverted-property)) parse-tree &key)
  "The case for \(:INVERTED-PROPERTY <name>) where <name> is a string."
  (declare #.*standard-optimize-settings*)
  (declare (special accumulate-start-p))
  (setq accumulate-start-p nil)
  (make-instance 'char-class :test-function (complement* (resolve-property (second parse-tree)))))

(defmethod convert-compound-parse-tree ((token (eql :flags)) parse-tree &key)
  "The case for \(:FLAGS {<flag>}*) where flag is a modifier symbol
like :CASE-INSENSITIVE-P."
  (declare #.*standard-optimize-settings*)
  ;; set/unset the flags corresponding to the symbols
  ;; following :FLAGS
  (mapc #'set-flag (rest parse-tree))
  ;; we're only interested in the side effect of
  ;; setting/unsetting the flags and turn this syntactical
  ;; construct into a VOID object which'll be optimized
  ;; away when creating the matcher
  (make-instance 'void))

(defgeneric convert-simple-parse-tree (parse-tree)
  (declare #.*standard-optimize-settings*)
  (:documentation "Helper function for CONVERT-AUX which converts
parse trees which are atoms.")
  (:method ((parse-tree (eql :void)))
   (declare #.*standard-optimize-settings*)
   (make-instance 'void))
  (:method ((parse-tree (eql :word-boundary)))
   (declare #.*standard-optimize-settings*)
   (make-instance 'word-boundary :negatedp nil))
  (:method ((parse-tree (eql :non-word-boundary)))
   (declare #.*standard-optimize-settings*)
   (make-instance 'word-boundary :negatedp t))
  (:method ((parse-tree (eql :everything)))
   (declare #.*standard-optimize-settings*)
   (declare (special flags accumulate-start-p))
   (setq accumulate-start-p nil)
   (make-instance 'everything :single-line-p (single-line-mode-p flags)))
  (:method ((parse-tree (eql :digit-class)))
   (declare #.*standard-optimize-settings*)
   (declare (special accumulate-start-p))
   (setq accumulate-start-p nil)
   (make-instance 'char-class :test-function #'digit-char-p))
  (:method ((parse-tree (eql :word-char-class)))
   (declare #.*standard-optimize-settings*)
   (declare (special accumulate-start-p))
   (setq accumulate-start-p nil)
   (make-instance 'char-class :test-function #'word-char-p))
  (:method ((parse-tree (eql :whitespace-char-class)))
   (declare #.*standard-optimize-settings*)
   (declare (special accumulate-start-p))
   (setq accumulate-start-p nil)
   (make-instance 'char-class :test-function #'whitespacep))
  (:method ((parse-tree (eql :non-digit-class)))
   (declare #.*standard-optimize-settings*)
   (declare (special accumulate-start-p))
   (setq accumulate-start-p nil)
   (make-instance 'char-class :test-function (complement* #'digit-char-p)))
  (:method ((parse-tree (eql :non-word-char-class)))
   (declare #.*standard-optimize-settings*)
   (declare (special accumulate-start-p))
   (setq accumulate-start-p nil)
   (make-instance 'char-class :test-function (complement* #'word-char-p)))
  (:method ((parse-tree (eql :non-whitespace-char-class)))
   (declare #.*standard-optimize-settings*)
   (declare (special accumulate-start-p))
   (setq accumulate-start-p nil)
   (make-instance 'char-class :test-function (complement* #'whitespacep)))
  (:method ((parse-tree (eql :start-anchor)))
   ;; Perl's "^"
   (declare #.*standard-optimize-settings*)
   (declare (special flags))
   (make-instance 'anchor :startp t :multi-line-p (multi-line-mode-p flags)))
  (:method ((parse-tree (eql :end-anchor)))
   ;; Perl's "$"
   (declare #.*standard-optimize-settings*)
   (declare (special flags))
   (make-instance 'anchor :startp nil :multi-line-p (multi-line-mode-p flags)))
  (:method ((parse-tree (eql :modeless-start-anchor)))
   ;; Perl's "\A"
   (declare #.*standard-optimize-settings*)
   (make-instance 'anchor :startp t))
  (:method ((parse-tree (eql :modeless-end-anchor)))
   ;; Perl's "$\Z"
   (declare #.*standard-optimize-settings*)
   (make-instance 'anchor :startp nil))
  (:method ((parse-tree (eql :modeless-end-anchor-no-newline)))
   ;; Perl's "$\z"
   (declare #.*standard-optimize-settings*)
   (make-instance 'anchor :startp nil :no-newline-p t))
  (:method ((parse-tree (eql :case-insensitive-p)))
   (declare #.*standard-optimize-settings*)
   (set-flag parse-tree)
   (make-instance 'void))
  (:method ((parse-tree (eql :case-sensitive-p)))
   (declare #.*standard-optimize-settings*)
   (set-flag parse-tree)
   (make-instance 'void))
  (:method ((parse-tree (eql :multi-line-mode-p)))
   (declare #.*standard-optimize-settings*)
   (set-flag parse-tree)
   (make-instance 'void))
  (:method ((parse-tree (eql :not-multi-line-mode-p)))
   (declare #.*standard-optimize-settings*)
   (set-flag parse-tree)
   (make-instance 'void))
  (:method ((parse-tree (eql :single-line-mode-p)))
   (declare #.*standard-optimize-settings*)
   (set-flag parse-tree)
   (make-instance 'void))
  (:method ((parse-tree (eql :not-single-line-mode-p)))
   (declare #.*standard-optimize-settings*)
   (set-flag parse-tree)
   (make-instance 'void)))

(defmethod convert-simple-parse-tree ((parse-tree string))
  (declare #.*standard-optimize-settings*)
  (declare (special flags))
  ;; turn strings into STR objects and try to accumulate into
  ;; STARTS-WITH
  (let ((str (make-instance 'str
                            :str parse-tree
                            :case-insensitive-p (case-insensitive-mode-p flags))))
    (maybe-accumulate str)
    str))

(defmethod convert-simple-parse-tree ((parse-tree character))
  (declare #.*standard-optimize-settings*)
  ;; dispatch to the method for strings
  (convert-simple-parse-tree (string parse-tree)))
        
(defmethod convert-simple-parse-tree (parse-tree)
  "The default method - check if there's a translation."
  (declare #.*standard-optimize-settings*)
  (let ((translation (and (symbolp parse-tree) (parse-tree-synonym parse-tree))))
    (if translation
      (convert-aux (copy-tree translation))
      (signal-syntax-error "Unknown token ~A in parse tree." parse-tree))))

(defun convert (parse-tree)
  "Converts the parse tree PARSE-TREE into an equivalent REGEX object
and returns three values: the REGEX object, the number of registers
seen and an object the regex starts with which is either a STR object
or an EVERYTHING object \(if the regex starts with something like
\".*\") or NIL."
  (declare #.*standard-optimize-settings*)
  ;; this function basically just initializes the special variables
  ;; and then calls CONVERT-AUX to do all the work
  (let* ((flags (list nil nil nil))
         (reg-num 0)
         reg-names
         named-reg-seen
         (accumulate-start-p t)
         starts-with
         (max-back-ref 0)
         (converted-parse-tree (convert-aux parse-tree)))
    (declare (special flags reg-num reg-names named-reg-seen
                      accumulate-start-p starts-with max-back-ref))
    ;; make sure we don't reference registers which aren't there
    (when (> (the fixnum max-back-ref)
             (the fixnum reg-num))
      (signal-syntax-error "Backreference to register ~A which has not been defined." max-back-ref))
    (when (typep starts-with 'str)
      (setf (slot-value starts-with 'str)
              (coerce (slot-value starts-with 'str)
                      #+:lispworks 'lw:simple-text-string
                      #-:lispworks 'simple-string)))
    (values converted-parse-tree reg-num starts-with
            ;; we can't simply use *ALLOW-NAMED-REGISTERS*
            ;; since parse-tree syntax ignores it
            (when named-reg-seen
              (nreverse reg-names)))))
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/optimize.lisp,v 1.36 2009/09/17 19:17:31 edi Exp $

;;; This file contains optimizations which can be applied to converted
;;; parse trees.

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

(defgeneric flatten (regex)
  (declare #.*standard-optimize-settings*)
  (:documentation "Merges adjacent sequences and alternations, i.e. it
transforms #<SEQ #<STR \"a\"> #<SEQ #<STR \"b\"> #<STR \"c\">>> to
#<SEQ #<STR \"a\"> #<STR \"b\"> #<STR \"c\">>. This is a destructive
operation on REGEX."))

(defmethod flatten ((seq seq))
  (declare #.*standard-optimize-settings*)
  ;; this looks more complicated than it is because we modify SEQ in
  ;; place to avoid unnecessary consing
  (let ((elements-rest (elements seq)))
    (loop
      (unless elements-rest
        (return))
      (let ((flattened-element (flatten (car elements-rest)))
            (next-elements-rest (cdr elements-rest)))
        (cond ((typep flattened-element 'seq)
                ;; FLATTENED-ELEMENT is a SEQ object, so we "splice"
                ;; it into out list of elements
                (let ((flattened-element-elements
                        (elements flattened-element)))
                  (setf (car elements-rest)
                          (car flattened-element-elements)
                        (cdr elements-rest)
                          (nconc (cdr flattened-element-elements)
                                 (cdr elements-rest)))))
              (t
                ;; otherwise we just replace the current element with
                ;; its flattened counterpart
                (setf (car elements-rest) flattened-element)))
        (setq elements-rest next-elements-rest))))
  (let ((elements (elements seq)))
    (cond ((cadr elements)
            seq)
          ((cdr elements)
            (first elements))
          (t (make-instance 'void)))))

(defmethod flatten ((alternation alternation))
  (declare #.*standard-optimize-settings*)
  ;; same algorithm as above
  (let ((choices-rest (choices alternation)))
    (loop
      (unless choices-rest
        (return))
      (let ((flattened-choice (flatten (car choices-rest)))
            (next-choices-rest (cdr choices-rest)))
        (cond ((typep flattened-choice 'alternation)
                (let ((flattened-choice-choices
                        (choices flattened-choice)))
                  (setf (car choices-rest)
                          (car flattened-choice-choices)
                        (cdr choices-rest)
                          (nconc (cdr flattened-choice-choices)
                                 (cdr choices-rest)))))
              (t
                (setf (car choices-rest) flattened-choice)))
        (setq choices-rest next-choices-rest))))
  (let ((choices (choices alternation)))
    (cond ((cadr choices)
            alternation)
          ((cdr choices)
            (first choices))
          (t (signal-syntax-error "Encountered alternation without choices.")))))

(defmethod flatten ((branch branch))
  (declare #.*standard-optimize-settings*)
  (with-slots (test then-regex else-regex)
      branch
    (setq test
            (if (numberp test)
              test
              (flatten test))
          then-regex (flatten then-regex)
          else-regex (flatten else-regex))
    branch))

(defmethod flatten ((regex regex))
  (declare #.*standard-optimize-settings*)
  (typecase regex
    ((or repetition register lookahead lookbehind standalone)
      ;; if REGEX contains exactly one inner REGEX object flatten it
      (setf (regex regex)
              (flatten (regex regex)))
      regex)
    (t
      ;; otherwise (ANCHOR, BACK-REFERENCE, CHAR-CLASS, EVERYTHING,
      ;; LOOKAHEAD, LOOKBEHIND, STR, VOID, FILTER, and WORD-BOUNDARY)
      ;; do nothing
      regex)))

(defgeneric gather-strings (regex)
  (declare #.*standard-optimize-settings*)
  (:documentation "Collects adjacent strings or characters into one
string provided they have the same case mode. This is a destructive
operation on REGEX."))

(defmethod gather-strings ((seq seq))
  (declare #.*standard-optimize-settings*)
  ;; note that GATHER-STRINGS is to be applied after FLATTEN, i.e. it
  ;; expects SEQ to be flattened already; in particular, SEQ cannot be
  ;; empty and cannot contain embedded SEQ objects
  (let* ((start-point (cons nil (elements seq)))
         (curr-point start-point)
         old-case-mode
         collector
         collector-start
         (collector-length 0)
         skip)
    (declare (fixnum collector-length))
    (loop
      (let ((elements-rest (cdr curr-point)))
        (unless elements-rest
          (return))
        (let* ((element (car elements-rest))
               (case-mode (case-mode element old-case-mode)))
          (cond ((and case-mode
                      (eq case-mode old-case-mode))
                  ;; if ELEMENT is a STR and we have collected a STR of
                  ;; the same case mode in the last iteration we
                  ;; concatenate ELEMENT onto COLLECTOR and remember the
                  ;; value of its SKIP slot
                  (let ((old-collector-length collector-length))
                    (unless (and (adjustable-array-p collector)
                                 (array-has-fill-pointer-p collector))
                      (setq collector
                              (make-array collector-length
                                          :initial-contents collector
                                          :element-type 'character
                                          :fill-pointer t
                                          :adjustable t)
                            collector-start nil))
                    (adjust-array collector
                                  (incf collector-length (len element))
                                  :fill-pointer t)
                    (setf (subseq collector
                                  old-collector-length)
                            (str element)
                          ;; it suffices to remember the last SKIP slot
                          ;; because due to the way MAYBE-ACCUMULATE
                          ;; works adjacent STR objects have the same
                          ;; SKIP value
                          skip (skip element)))
                  (setf (cdr curr-point) (cdr elements-rest)))
                (t
                  (let ((collected-string
                          (cond (collector-start
                                  collector-start)
                                (collector
                                  ;; if we have collected something already
                                  ;; we convert it into a STR
                                  (make-instance 'str
                                                 :skip skip
                                                 :str collector
                                                 :case-insensitive-p
                                                 (eq old-case-mode
                                                     :case-insensitive)))
                                (t nil))))
                    (cond (case-mode
                            ;; if ELEMENT is a string with a different case
                            ;; mode than the last one we have either just
                            ;; converted COLLECTOR into a STR or COLLECTOR
                            ;; is still empty; in both cases we can now
                            ;; begin to fill it anew
                            (setq collector (str element)
                                  collector-start element
                                  ;; and we remember the SKIP value as above
                                  skip (skip element)
                                  collector-length (len element))
                            (cond (collected-string
                                    (setf (car elements-rest)
                                            collected-string
                                          curr-point
                                            (cdr curr-point)))
                                  (t
                                    (setf (cdr curr-point)
                                            (cdr elements-rest)))))
                          (t
                            ;; otherwise this is not a STR so we apply
                            ;; GATHER-STRINGS to it and collect it directly
                            ;; into RESULT
                            (cond (collected-string
                                    (setf (car elements-rest)
                                            collected-string
                                          curr-point
                                            (cdr curr-point)
                                          (cdr curr-point)
                                            (cons (gather-strings element)
                                                  (cdr curr-point))
                                          curr-point
                                            (cdr curr-point)))
                                  (t
                                    (setf (car elements-rest)
                                            (gather-strings element)
                                          curr-point
                                            (cdr curr-point))))
                            ;; we also have to empty COLLECTOR here in case
                            ;; it was still filled from the last iteration
                            (setq collector nil
                                  collector-start nil))))))
          (setq old-case-mode case-mode))))
    (when collector
      (setf (cdr curr-point)
              (cons
               (make-instance 'str
                              :skip skip
                              :str collector
                              :case-insensitive-p
                              (eq old-case-mode
                                  :case-insensitive))
               nil)))
    (setf (elements seq) (cdr start-point))
    seq))

(defmethod gather-strings ((alternation alternation))
  (declare #.*standard-optimize-settings*)
  ;; loop ON the choices of ALTERNATION so we can modify them directly
  (loop for choices-rest on (choices alternation)
        while choices-rest
        do (setf (car choices-rest)
                   (gather-strings (car choices-rest))))
  alternation)

(defmethod gather-strings ((branch branch))
  (declare #.*standard-optimize-settings*)
  (with-slots (test then-regex else-regex)
      branch
    (setq test
            (if (numberp test)
              test
              (gather-strings test))
          then-regex (gather-strings then-regex)
          else-regex (gather-strings else-regex))
    branch))

(defmethod gather-strings ((regex regex))
  (declare #.*standard-optimize-settings*)
  (typecase regex
    ((or repetition register lookahead lookbehind standalone)
      ;; if REGEX contains exactly one inner REGEX object apply
      ;; GATHER-STRINGS to it
      (setf (regex regex)
              (gather-strings (regex regex)))
      regex)
    (t
      ;; otherwise (ANCHOR, BACK-REFERENCE, CHAR-CLASS, EVERYTHING,
      ;; LOOKAHEAD, LOOKBEHIND, STR, VOID, FILTER, and WORD-BOUNDARY)
      ;; do nothing
      regex)))

;; Note that START-ANCHORED-P will be called after FLATTEN and GATHER-STRINGS.

(defgeneric start-anchored-p (regex &optional in-seq-p)
  (declare #.*standard-optimize-settings*)
  (:documentation "Returns T if REGEX starts with a \"real\" start
anchor, i.e. one that's not in multi-line mode, NIL otherwise. If
IN-SEQ-P is true the function will return :ZERO-LENGTH if REGEX is a
zero-length assertion."))

(defmethod start-anchored-p ((seq seq) &optional in-seq-p)
  (declare (ignore in-seq-p))
  ;; note that START-ANCHORED-P is to be applied after FLATTEN and
  ;; GATHER-STRINGS, i.e. SEQ cannot be empty and cannot contain
  ;; embedded SEQ objects
  (loop for element in (elements seq)
        for anchored-p = (start-anchored-p element t)
        ;; skip zero-length elements because they won't affect the
        ;; "anchoredness" of the sequence
        while (eq anchored-p :zero-length)
        finally (return (and anchored-p (not (eq anchored-p :zero-length))))))

(defmethod start-anchored-p ((alternation alternation) &optional in-seq-p)
  (declare #.*standard-optimize-settings*)
  (declare (ignore in-seq-p))
  ;; clearly an alternation can only be start-anchored if all of its
  ;; choices are start-anchored
  (loop for choice in (choices alternation)
        always (start-anchored-p choice)))

(defmethod start-anchored-p ((branch branch) &optional in-seq-p)
  (declare #.*standard-optimize-settings*)
  (declare (ignore in-seq-p))
  (and (start-anchored-p (then-regex branch))
       (start-anchored-p (else-regex branch))))

(defmethod start-anchored-p ((repetition repetition) &optional in-seq-p)
  (declare #.*standard-optimize-settings*)
  (declare (ignore in-seq-p))
  ;; well, this wouldn't make much sense, but anyway...
  (and (plusp (minimum repetition))
       (start-anchored-p (regex repetition))))

(defmethod start-anchored-p ((register register) &optional in-seq-p)
  (declare #.*standard-optimize-settings*)
  (declare (ignore in-seq-p))
  (start-anchored-p (regex register)))

(defmethod start-anchored-p ((standalone standalone) &optional in-seq-p)
  (declare #.*standard-optimize-settings*)
  (declare (ignore in-seq-p))
  (start-anchored-p (regex standalone)))

(defmethod start-anchored-p ((anchor anchor) &optional in-seq-p)
  (declare #.*standard-optimize-settings*)
  (declare (ignore in-seq-p))
  (and (startp anchor)
       (not (multi-line-p anchor))))

(defmethod start-anchored-p ((regex regex) &optional in-seq-p)
  (declare #.*standard-optimize-settings*)
  (typecase regex
    ((or lookahead lookbehind word-boundary void)
      ;; zero-length assertions
      (if in-seq-p
        :zero-length
        nil))
    (filter
      (if (and in-seq-p
               (len regex)
               (zerop (len regex)))
        :zero-length
        nil))
    (t
      ;; BACK-REFERENCE, CHAR-CLASS, EVERYTHING, and STR
      nil)))

;; Note that END-STRING-AUX will be called after FLATTEN and GATHER-STRINGS.

(defgeneric end-string-aux (regex &optional old-case-insensitive-p)
  (declare #.*standard-optimize-settings*)
  (:documentation "Returns the constant string (if it exists) REGEX
ends with wrapped into a STR object, otherwise NIL.
OLD-CASE-INSENSITIVE-P is the CASE-INSENSITIVE-P slot of the last STR
collected or :VOID if no STR has been collected yet. (This is a helper
function called by END-STRING.)"))

(defmethod end-string-aux ((str str)
                           &optional (old-case-insensitive-p :void))
  (declare #.*standard-optimize-settings*)
  (declare (special last-str))
  (cond ((and (not (skip str))          ; avoid constituents of STARTS-WITH
              ;; only use STR if nothing has been collected yet or if
              ;; the collected string has the same value for
              ;; CASE-INSENSITIVE-P
              (or (eq old-case-insensitive-p :void)
                  (eq (case-insensitive-p str) old-case-insensitive-p)))
          (setf last-str str
                ;; set the SKIP property of this STR
                (skip str) t)
          str)
        (t nil)))

(defmethod end-string-aux ((seq seq)
                           &optional (old-case-insensitive-p :void))
  (declare #.*standard-optimize-settings*)
  (declare (special continuep))
  (let (case-insensitive-p
        concatenated-string
        concatenated-start
        (concatenated-length 0))
    (declare (fixnum concatenated-length))
    (loop for element in (reverse (elements seq))
          ;; remember the case-(in)sensitivity of the last relevant
          ;; STR object
          for loop-old-case-insensitive-p = old-case-insensitive-p
            then (if skip
                   loop-old-case-insensitive-p
                   (case-insensitive-p element-end))
          ;; the end-string of the current element
          for element-end = (end-string-aux element
                                            loop-old-case-insensitive-p)
          ;; whether we encountered a zero-length element
          for skip = (if element-end
                       (zerop (len element-end))
                       nil)
          ;; set CONTINUEP to NIL if we have to stop collecting to
          ;; alert END-STRING-AUX methods on enclosing SEQ objects
          unless element-end
            do (setq continuep nil)
          ;; end loop if we neither got a STR nor a zero-length
          ;; element
          while element-end
          ;; only collect if not zero-length
          unless skip
            do (cond (concatenated-string
                       (when concatenated-start
                         (setf concatenated-string
                                 (make-array concatenated-length
                                             :initial-contents (reverse (str concatenated-start))
                                             :element-type 'character
                                             :fill-pointer t
                                             :adjustable t)
                               concatenated-start nil))
                       (let ((len (len element-end))
                             (str (str element-end)))
                         (declare (fixnum len))
                         (incf concatenated-length len)
                         (loop for i of-type fixnum downfrom (1- len) to 0
                               do (vector-push-extend (char str i)
                                                      concatenated-string))))
                     (t
                       (setf concatenated-string
                               t
                             concatenated-start
                               element-end
                             concatenated-length
                               (len element-end)
                             case-insensitive-p
                               (case-insensitive-p element-end))))
          ;; stop collecting if END-STRING-AUX on inner SEQ has said so
          while continuep)
    (cond ((zerop concatenated-length)
            ;; don't bother to return zero-length strings
            nil)
          (concatenated-start
            concatenated-start)
          (t
            (make-instance 'str
                           :str (nreverse concatenated-string)
                           :case-insensitive-p case-insensitive-p)))))

(defmethod end-string-aux ((register register)
                           &optional (old-case-insensitive-p :void))
  (declare #.*standard-optimize-settings*)
  (end-string-aux (regex register) old-case-insensitive-p))
    
(defmethod end-string-aux ((standalone standalone)
                           &optional (old-case-insensitive-p :void))
  (declare #.*standard-optimize-settings*)
  (end-string-aux (regex standalone) old-case-insensitive-p))
    
(defmethod end-string-aux ((regex regex)
                           &optional (old-case-insensitive-p :void))
  (declare #.*standard-optimize-settings*)
  (declare (special last-str end-anchored-p continuep))
  (typecase regex
    ((or anchor lookahead lookbehind word-boundary void)
      ;; a zero-length REGEX object - for the sake of END-STRING-AUX
      ;; this is a zero-length string
      (when (and (typep regex 'anchor)
                 (not (startp regex))
                 (or (no-newline-p regex)
                     (not (multi-line-p regex)))
                 (eq old-case-insensitive-p :void))
        ;; if this is a "real" end-anchor and we haven't collected
        ;; anything so far we can set END-ANCHORED-P (where 1 or 0
        ;; indicate whether we accept a #\Newline at the end or not)
        (setq end-anchored-p (if (no-newline-p regex) 0 1)))
      (make-instance 'str
                     :str ""
                     :case-insensitive-p :void))
    (t
      ;; (ALTERNATION, BACK-REFERENCE, BRANCH, CHAR-CLASS, EVERYTHING,
      ;; REPETITION, FILTER)
      nil)))

(defun end-string (regex)
  (declare (special end-string-offset))
  (declare #.*standard-optimize-settings*)
  "Returns the constant string (if it exists) REGEX ends with wrapped
into a STR object, otherwise NIL."
  ;; LAST-STR points to the last STR object (seen from the end) that's
  ;; part of END-STRING; CONTINUEP is set to T if we stop collecting
  ;; in the middle of a SEQ
  (let ((continuep t)
        last-str)
    (declare (special continuep last-str))
    (prog1
      (end-string-aux regex)
      (when last-str
        ;; if we've found something set the START-OF-END-STRING-P of
        ;; the leftmost STR collected accordingly and remember the
        ;; OFFSET of this STR (in a special variable provided by the
        ;; caller of this function)
        (setf (start-of-end-string-p last-str) t
              end-string-offset (offset last-str))))))

(defgeneric compute-min-rest (regex current-min-rest)
  (declare #.*standard-optimize-settings*)
  (:documentation "Returns the minimal length of REGEX plus
CURRENT-MIN-REST. This is similar to REGEX-MIN-LENGTH except that it
recurses down into REGEX and sets the MIN-REST slots of REPETITION
objects."))

(defmethod compute-min-rest ((seq seq) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (loop for element in (reverse (elements seq))
        for last-min-rest = current-min-rest then this-min-rest
        for this-min-rest = (compute-min-rest element last-min-rest)
        finally (return this-min-rest)))
    
(defmethod compute-min-rest ((alternation alternation) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (loop for choice in (choices alternation)
        minimize (compute-min-rest choice current-min-rest)))

(defmethod compute-min-rest ((branch branch) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (min (compute-min-rest (then-regex branch) current-min-rest)
       (compute-min-rest (else-regex branch) current-min-rest)))

(defmethod compute-min-rest ((str str) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (+ current-min-rest (len str)))
    
(defmethod compute-min-rest ((filter filter) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (+ current-min-rest (or (len filter) 0)))
    
(defmethod compute-min-rest ((repetition repetition) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (setf (min-rest repetition) current-min-rest)
  (compute-min-rest (regex repetition) current-min-rest)
  (+ current-min-rest (* (minimum repetition) (min-len repetition))))

(defmethod compute-min-rest ((register register) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (compute-min-rest (regex register) current-min-rest))
    
(defmethod compute-min-rest ((standalone standalone) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (declare (ignore current-min-rest))
  (compute-min-rest (regex standalone) 0))
    
(defmethod compute-min-rest ((lookahead lookahead) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (compute-min-rest (regex lookahead) 0)
  current-min-rest)
    
(defmethod compute-min-rest ((lookbehind lookbehind) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (compute-min-rest (regex lookbehind) (+ current-min-rest (len lookbehind)))
  current-min-rest)
    
(defmethod compute-min-rest ((regex regex) current-min-rest)
  (declare #.*standard-optimize-settings*)
  (typecase regex
    ((or char-class everything)
      (1+ current-min-rest))
    (t
      ;; zero min-len and no embedded regexes (ANCHOR,
      ;; BACK-REFERENCE, VOID, and WORD-BOUNDARY)
      current-min-rest)))
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/closures.lisp,v 1.45 2009/09/17 19:17:30 edi Exp $

;;; Here we create the closures which together build the final
;;; scanner.

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

(declaim (inline *string*= *string*-equal))
(defun *string*= (string2 start1 end1 start2 end2)
  "Like STRING=, i.e. compares the special string *STRING* from START1
to END1 with STRING2 from START2 to END2. Note that there's no
boundary check - this has to be implemented by the caller."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum start1 end1 start2 end2))
  (loop for string1-idx of-type fixnum from start1 below end1
        for string2-idx of-type fixnum from start2 below end2
        always (char= (schar *string* string1-idx)
                      (schar string2 string2-idx))))

(defun *string*-equal (string2 start1 end1 start2 end2)
  "Like STRING-EQUAL, i.e. compares the special string *STRING* from
START1 to END1 with STRING2 from START2 to END2. Note that there's no
boundary check - this has to be implemented by the caller."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum start1 end1 start2 end2))
  (loop for string1-idx of-type fixnum from start1 below end1
        for string2-idx of-type fixnum from start2 below end2
        always (char-equal (schar *string* string1-idx)
                           (schar string2 string2-idx))))

(defgeneric create-matcher-aux (regex next-fn)
  (declare #.*standard-optimize-settings*)
  (:documentation "Creates a closure which takes one parameter,
START-POS, and tests whether REGEX can match *STRING* at START-POS
such that the call to NEXT-FN after the match would succeed."))
                
(defmethod create-matcher-aux ((seq seq) next-fn)
  (declare #.*standard-optimize-settings*)
  ;; the closure for a SEQ is a chain of closures for the elements of
  ;; this sequence which call each other in turn; the last closure
  ;; calls NEXT-FN
  (loop for element in (reverse (elements seq))
        for curr-matcher = next-fn then next-matcher
        for next-matcher = (create-matcher-aux element curr-matcher)
        finally (return next-matcher)))

(defmethod create-matcher-aux ((alternation alternation) next-fn)
  (declare #.*standard-optimize-settings*)
  ;; first create closures for all alternations of ALTERNATION
  (let ((all-matchers (mapcar #'(lambda (choice)
                                  (create-matcher-aux choice next-fn))
                              (choices alternation))))
    ;; now create a closure which checks if one of the closures
    ;; created above can succeed
    (lambda (start-pos)
      (declare (fixnum start-pos))
      (loop for matcher in all-matchers
            thereis (funcall (the function matcher) start-pos)))))

(defmethod create-matcher-aux ((register register) next-fn)
  (declare #.*standard-optimize-settings*)
  ;; the position of this REGISTER within the whole regex; we start to
  ;; count at 0
  (let ((num (num register)))
    (declare (fixnum num))
    ;; STORE-END-OF-REG is a thin wrapper around NEXT-FN which will
    ;; update the corresponding values of *REGS-START* and *REGS-END*
    ;; after the inner matcher has succeeded
    (flet ((store-end-of-reg (start-pos)
               (declare (fixnum start-pos)
                        (function next-fn))
               (setf (svref *reg-starts* num) (svref *regs-maybe-start* num)
                     (svref *reg-ends* num) start-pos)
           (funcall next-fn start-pos)))
      ;; the inner matcher is a closure corresponding to the regex
      ;; wrapped by this REGISTER
      (let ((inner-matcher (create-matcher-aux (regex register)
                                               #'store-end-of-reg)))
        (declare (function inner-matcher))
        ;; here comes the actual closure for REGISTER
        (lambda (start-pos)
          (declare (fixnum start-pos))
          ;; remember the old values of *REGS-START* and friends in
          ;; case we cannot match
          (let ((old-*reg-starts* (svref *reg-starts* num))
                (old-*regs-maybe-start* (svref *regs-maybe-start* num))
                (old-*reg-ends* (svref *reg-ends* num)))
            ;; we cannot use *REGS-START* here because Perl allows
            ;; regular expressions like /(a|\1x)*/
            (setf (svref *regs-maybe-start* num) start-pos)
            (let ((next-pos (funcall inner-matcher start-pos)))
              (unless next-pos
                ;; restore old values on failure
                (setf (svref *reg-starts* num) old-*reg-starts*
                      (svref *regs-maybe-start* num) old-*regs-maybe-start*
                      (svref *reg-ends* num) old-*reg-ends*))
              next-pos)))))))

(defmethod create-matcher-aux ((lookahead lookahead) next-fn)
  (declare #.*standard-optimize-settings*)
  ;; create a closure which just checks for the inner regex and
  ;; doesn't care about NEXT-FN
  (let ((test-matcher (create-matcher-aux (regex lookahead) #'identity)))
    (declare (function next-fn test-matcher))
    (if (positivep lookahead)
      ;; positive look-ahead: check success of inner regex, then call
      ;; NEXT-FN
      (lambda (start-pos)
        (and (funcall test-matcher start-pos)
             (funcall next-fn start-pos)))
      ;; negative look-ahead: check failure of inner regex, then call
      ;; NEXT-FN
      (lambda (start-pos)
        (and (not (funcall test-matcher start-pos))
             (funcall next-fn start-pos))))))

(defmethod create-matcher-aux ((lookbehind lookbehind) next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((len (len lookbehind))
        ;; create a closure which just checks for the inner regex and
        ;; doesn't care about NEXT-FN
        (test-matcher (create-matcher-aux (regex lookbehind) #'identity)))
    (declare (function next-fn test-matcher)
             (fixnum len))
    (if (positivep lookbehind)
      ;; positive look-behind: check success of inner regex (if we're
      ;; far enough from the start of *STRING*), then call NEXT-FN
      (lambda (start-pos)
        (declare (fixnum start-pos))
        (and (>= (- start-pos (or *real-start-pos* *start-pos*)) len)
             (funcall test-matcher (- start-pos len))
             (funcall next-fn start-pos)))
      ;; negative look-behind: check failure of inner regex (if we're
      ;; far enough from the start of *STRING*), then call NEXT-FN
      (lambda (start-pos)
        (declare (fixnum start-pos))
        (and (or (< (- start-pos (or *real-start-pos* *start-pos*)) len)
                 (not (funcall test-matcher (- start-pos len))))
             (funcall next-fn start-pos))))))

(defmacro insert-char-class-tester ((char-class chr-expr) &body body)
  "Utility macro to replace each occurence of '\(CHAR-CLASS-TEST)
within BODY with the correct test (corresponding to CHAR-CLASS)
against CHR-EXPR."
  (with-rebinding (char-class)
    (with-unique-names (test-function)
      (flet ((substitute-char-class-tester (new)
               (subst new '(char-class-test) body
                      :test #'equalp)))
        `(let ((,test-function (test-function ,char-class)))
           ,@(substitute-char-class-tester
              `(funcall ,test-function ,chr-expr)))))))

(defmethod create-matcher-aux ((char-class char-class) next-fn)
  (declare #.*standard-optimize-settings*)
  (declare (function next-fn))
  ;; insert a test against the current character within *STRING*
  (insert-char-class-tester (char-class (schar *string* start-pos))
    (lambda (start-pos)
      (declare (fixnum start-pos))
      (and (< start-pos *end-pos*)
           (char-class-test)
           (funcall next-fn (1+ start-pos))))))

(defmethod create-matcher-aux ((str str) next-fn)
  (declare #.*standard-optimize-settings*)
  (declare (fixnum *end-string-pos*)
           (function next-fn)
           ;; this special value is set by CREATE-SCANNER when the
           ;; closures are built
           (special end-string))
  (let* ((len (len str))
         (case-insensitive-p (case-insensitive-p str))
         (start-of-end-string-p (start-of-end-string-p str))
         (skip (skip str))
         (str (str str))
         (chr (schar str 0))
         (end-string (and end-string (str end-string)))
         (end-string-len (if end-string
                           (length end-string)
                           nil)))
    (declare (fixnum len))
    (cond ((and start-of-end-string-p case-insensitive-p)
            ;; closure for the first STR which belongs to the constant
            ;; string at the end of the regular expression;
            ;; case-insensitive version
            (lambda (start-pos)
              (declare (fixnum start-pos end-string-len))
              (let ((test-end-pos (+ start-pos end-string-len)))
                (declare (fixnum test-end-pos))
                ;; either we're at *END-STRING-POS* (which means that
                ;; it has already been confirmed that end-string
                ;; starts here) or we really have to test
                (and (or (= start-pos *end-string-pos*)
                         (and (<= test-end-pos *end-pos*)
                              (*string*-equal end-string start-pos test-end-pos
                                              0 end-string-len)))
                     (funcall next-fn (+ start-pos len))))))
          (start-of-end-string-p
            ;; closure for the first STR which belongs to the constant
            ;; string at the end of the regular expression;
            ;; case-sensitive version
            (lambda (start-pos)
              (declare (fixnum start-pos end-string-len))
              (let ((test-end-pos (+ start-pos end-string-len)))
                (declare (fixnum test-end-pos))
                ;; either we're at *END-STRING-POS* (which means that
                ;; it has already been confirmed that end-string
                ;; starts here) or we really have to test
                (and (or (= start-pos *end-string-pos*)
                         (and (<= test-end-pos *end-pos*)
                              (*string*= end-string start-pos test-end-pos
                                         0 end-string-len)))
                     (funcall next-fn (+ start-pos len))))))
          (skip
            ;; a STR which can be skipped because some other function
            ;; has already confirmed that it matches
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (funcall next-fn (+ start-pos len))))
          ((and (= len 1) case-insensitive-p)
            ;; STR represent exactly one character; case-insensitive
            ;; version
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (and (< start-pos *end-pos*)
                   (char-equal (schar *string* start-pos) chr)
                   (funcall next-fn (1+ start-pos)))))
          ((= len 1)
            ;; STR represent exactly one character; case-sensitive
            ;; version
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (and (< start-pos *end-pos*)
                   (char= (schar *string* start-pos) chr)
                   (funcall next-fn (1+ start-pos)))))
          (case-insensitive-p
            ;; general case, case-insensitive version
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (let ((next-pos (+ start-pos len)))
                (declare (fixnum next-pos))
                (and (<= next-pos *end-pos*)
                     (*string*-equal str start-pos next-pos 0 len)
                     (funcall next-fn next-pos)))))
          (t
            ;; general case, case-sensitive version
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (let ((next-pos (+ start-pos len)))
                (declare (fixnum next-pos))
                (and (<= next-pos *end-pos*)
                     (*string*= str start-pos next-pos 0 len)
                     (funcall next-fn next-pos))))))))

(declaim (inline word-boundary-p))
(defun word-boundary-p (start-pos)
  "Check whether START-POS is a word-boundary within *STRING*."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum start-pos))
  (let ((1-start-pos (1- start-pos))
        (*start-pos* (or *real-start-pos* *start-pos*)))
    ;; either the character before START-POS is a word-constituent and
    ;; the character at START-POS isn't...
    (or (and (or (= start-pos *end-pos*)
                 (and (< start-pos *end-pos*)
                      (not (word-char-p (schar *string* start-pos)))))
             (and (< 1-start-pos *end-pos*)
                  (<= *start-pos* 1-start-pos)
                  (word-char-p (schar *string* 1-start-pos))))
        ;; ...or vice versa
        (and (or (= start-pos *start-pos*)
                 (and (< 1-start-pos *end-pos*)
                      (<= *start-pos* 1-start-pos)
                      (not (word-char-p (schar *string* 1-start-pos)))))
             (and (< start-pos *end-pos*)
                  (word-char-p (schar *string* start-pos)))))))

(defmethod create-matcher-aux ((word-boundary word-boundary) next-fn)
  (declare #.*standard-optimize-settings*)
  (declare (function next-fn))
  (if (negatedp word-boundary)
    (lambda (start-pos)
      (and (not (word-boundary-p start-pos))
           (funcall next-fn start-pos)))
    (lambda (start-pos)
      (and (word-boundary-p start-pos)
           (funcall next-fn start-pos)))))

(defmethod create-matcher-aux ((everything everything) next-fn)
  (declare #.*standard-optimize-settings*)
  (declare (function next-fn))
  (if (single-line-p everything)
    ;; closure for single-line-mode: we really match everything, so we
    ;; just advance the index into *STRING* by one and carry on
    (lambda (start-pos)
      (declare (fixnum start-pos))
      (and (< start-pos *end-pos*)
           (funcall next-fn (1+ start-pos))))
    ;; not single-line-mode, so we have to make sure we don't match
    ;; #\Newline
    (lambda (start-pos)
      (declare (fixnum start-pos))
      (and (< start-pos *end-pos*)
           (char/= (schar *string* start-pos) #\Newline)
           (funcall next-fn (1+ start-pos))))))

(defmethod create-matcher-aux ((anchor anchor) next-fn)
  (declare #.*standard-optimize-settings*)
  (declare (function next-fn))
  (let ((startp (startp anchor))
        (multi-line-p (multi-line-p anchor)))
    (cond ((no-newline-p anchor)
            ;; this must be an end-anchor and it must be modeless, so
            ;; we just have to check whether START-POS equals
            ;; *END-POS*
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (and (= start-pos *end-pos*)
                   (funcall next-fn start-pos))))
          ((and startp multi-line-p)
            ;; a start-anchor in multi-line-mode: check if we're at
            ;; *START-POS* or if the last character was #\Newline
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (let ((*start-pos* (or *real-start-pos* *start-pos*)))
                (and (or (= start-pos *start-pos*)
                         (and (<= start-pos *end-pos*)
                              (> start-pos *start-pos*)
                              (char= #\Newline
                                     (schar *string* (1- start-pos)))))
                     (funcall next-fn start-pos)))))
          (startp
            ;; a start-anchor which is not in multi-line-mode, so just
            ;; check whether we're at *START-POS*
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (and (= start-pos (or *real-start-pos* *start-pos*))
                   (funcall next-fn start-pos))))
          (multi-line-p
            ;; an end-anchor in multi-line-mode: check if we're at
            ;; *END-POS* or if the character we're looking at is
            ;; #\Newline
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (and (or (= start-pos *end-pos*)
                       (and (< start-pos *end-pos*)
                            (char= #\Newline
                                   (schar *string* start-pos))))
                   (funcall next-fn start-pos))))
          (t
            ;; an end-anchor which is not in multi-line-mode, so just
            ;; check if we're at *END-POS* or if we're looking at
            ;; #\Newline and there's nothing behind it
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (and (or (= start-pos *end-pos*)
                       (and (= start-pos (1- *end-pos*))
                            (char= #\Newline
                                   (schar *string* start-pos))))
                   (funcall next-fn start-pos)))))))

(defmethod create-matcher-aux ((back-reference back-reference) next-fn)
  (declare #.*standard-optimize-settings*)
  (declare (function next-fn))
  ;; the position of the corresponding REGISTER within the whole
  ;; regex; we start to count at 0
  (let ((num (num back-reference)))
    (if (case-insensitive-p back-reference)
      ;; the case-insensitive version
      (lambda (start-pos)
        (declare (fixnum start-pos))
        (let ((reg-start (svref *reg-starts* num))
              (reg-end (svref *reg-ends* num)))
          ;; only bother to check if the corresponding REGISTER as
          ;; matched successfully already
          (and reg-start
               (let ((next-pos (+ start-pos (- (the fixnum reg-end)
                                               (the fixnum reg-start)))))
                 (declare (fixnum next-pos))
                 (and
                   (<= next-pos *end-pos*)
                   (*string*-equal *string* start-pos next-pos
                                   reg-start reg-end)
                   (funcall next-fn next-pos))))))
      ;; the case-sensitive version
      (lambda (start-pos)
        (declare (fixnum start-pos))
        (let ((reg-start (svref *reg-starts* num))
              (reg-end (svref *reg-ends* num)))
          ;; only bother to check if the corresponding REGISTER as
          ;; matched successfully already
          (and reg-start
               (let ((next-pos (+ start-pos (- (the fixnum reg-end)
                                               (the fixnum reg-start)))))
                 (declare (fixnum next-pos))
                 (and
                   (<= next-pos *end-pos*)
                   (*string*= *string* start-pos next-pos
                              reg-start reg-end)
                   (funcall next-fn next-pos)))))))))

(defmethod create-matcher-aux ((branch branch) next-fn)
  (declare #.*standard-optimize-settings*)
  (let* ((test (test branch))
         (then-matcher (create-matcher-aux (then-regex branch) next-fn))
         (else-matcher (create-matcher-aux (else-regex branch) next-fn)))
    (declare (function then-matcher else-matcher))
    (cond ((numberp test)
            (lambda (start-pos)
              (declare (fixnum test))
              (if (and (< test (length *reg-starts*))
                       (svref *reg-starts* test))
                (funcall then-matcher start-pos)
                (funcall else-matcher start-pos))))
          (t
            (let ((test-matcher (create-matcher-aux test #'identity)))
              (declare (function test-matcher))
              (lambda (start-pos)
                (if (funcall test-matcher start-pos)
                  (funcall then-matcher start-pos)
                  (funcall else-matcher start-pos))))))))

(defmethod create-matcher-aux ((standalone standalone) next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((inner-matcher (create-matcher-aux (regex standalone) #'identity)))
    (declare (function next-fn inner-matcher))
    (lambda (start-pos)
      (let ((next-pos (funcall inner-matcher start-pos)))
        (and next-pos
             (funcall next-fn next-pos))))))

(defmethod create-matcher-aux ((filter filter) next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((fn (fn filter)))
    (lambda (start-pos)
      (let ((next-pos (funcall fn start-pos)))
        (and next-pos
             (funcall next-fn next-pos))))))

(defmethod create-matcher-aux ((void void) next-fn)
  (declare #.*standard-optimize-settings*)
  ;; optimize away VOIDs: don't create a closure, just return NEXT-FN
  next-fn)
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/repetition-closures.lisp,v 1.34 2009/09/17 19:17:31 edi Exp $

;;; This is actually a part of closures.lisp which we put into a
;;; separate file because it is rather complex. We only deal with
;;; REPETITIONs here. Note that this part of the code contains some
;;; rather crazy micro-optimizations which were introduced to be as
;;; competitive with Perl as possible in tight loops.

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

(defmacro incf-after (place &optional (delta 1) &environment env)
  "Utility macro inspired by C's \"place++\", i.e. first return the
value of PLACE and afterwards increment it by DELTA."
  (with-unique-names (%temp)
    (multiple-value-bind (vars vals store-vars writer-form reader-form)
        (get-setf-expansion place env)
      `(let* (,@(mapcar #'list vars vals)
              (,%temp ,reader-form)
              (,(car store-vars) (+ ,%temp ,delta)))
        ,writer-form
        ,%temp))))

;; code for greedy repetitions with minimum zero

(defmacro greedy-constant-length-closure (check-curr-pos)
  "This is the template for simple greedy repetitions (where simple
means that the minimum number of repetitions is zero, that the inner
regex to be checked is of fixed length LEN, and that it doesn't
contain registers, i.e. there's no need for backtracking).
CHECK-CURR-POS is a form which checks whether the inner regex of the
repetition matches at CURR-POS."
  `(if maximum
    (lambda (start-pos)
      (declare (fixnum start-pos maximum))
      ;; because we know LEN we know in advance where to stop at the
      ;; latest; we also take into consideration MIN-REST, i.e. the
      ;; minimal length of the part behind the repetition
      (let ((target-end-pos (min (1+ (- *end-pos* len min-rest))
                                 ;; don't go further than MAXIMUM
                                 ;; repetitions, of course
                                 (+ start-pos
                                    (the fixnum (* len maximum)))))
            (curr-pos start-pos))
        (declare (fixnum target-end-pos curr-pos))
        (block greedy-constant-length-matcher
          ;; we use an ugly TAGBODY construct because this might be a
          ;; tight loop and this version is a bit faster than our LOOP
          ;; version (at least in CMUCL)
          (tagbody
            forward-loop
            ;; first go forward as far as possible, i.e. while
            ;; the inner regex matches
            (when (>= curr-pos target-end-pos)
              (go backward-loop))
            (when ,check-curr-pos
              (incf curr-pos len)
              (go forward-loop))
            backward-loop
            ;; now go back LEN steps each until we're able to match
            ;; the rest of the regex
            (when (< curr-pos start-pos)
              (return-from greedy-constant-length-matcher nil))
            (let ((result (funcall next-fn curr-pos)))
              (when result
                (return-from greedy-constant-length-matcher result)))
            (decf curr-pos len)
            (go backward-loop)))))
    ;; basically the same code; it's just a bit easier because we're
    ;; not bounded by MAXIMUM
    (lambda (start-pos)
      (declare (fixnum start-pos))
      (let ((target-end-pos (1+ (- *end-pos* len min-rest)))
            (curr-pos start-pos))
        (declare (fixnum target-end-pos curr-pos))
        (block greedy-constant-length-matcher
          (tagbody
            forward-loop
            (when (>= curr-pos target-end-pos)
              (go backward-loop))
            (when ,check-curr-pos
              (incf curr-pos len)
              (go forward-loop))
            backward-loop
            (when (< curr-pos start-pos)
              (return-from greedy-constant-length-matcher nil))
            (let ((result (funcall next-fn curr-pos)))
              (when result
                (return-from greedy-constant-length-matcher result)))
            (decf curr-pos len)
            (go backward-loop)))))))

(defun create-greedy-everything-matcher (maximum min-rest next-fn)
  "Creates a closure which just matches as far ahead as possible,
i.e. a closure for a dot in single-line mode."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum min-rest) (function next-fn))
  (if maximum
    (lambda (start-pos)
      (declare (fixnum start-pos maximum))
      ;; because we know LEN we know in advance where to stop at the
      ;; latest; we also take into consideration MIN-REST, i.e. the
      ;; minimal length of the part behind the repetition
      (let ((target-end-pos (min (+ start-pos maximum)
                                 (- *end-pos* min-rest))))
        (declare (fixnum target-end-pos))
        ;; start from the highest possible position and go backward
        ;; until we're able to match the rest of the regex
        (loop for curr-pos of-type fixnum from target-end-pos downto start-pos
              thereis (funcall next-fn curr-pos))))
    ;; basically the same code; it's just a bit easier because we're
    ;; not bounded by MAXIMUM
    (lambda (start-pos)
      (declare (fixnum start-pos))
      (let ((target-end-pos (- *end-pos* min-rest)))
        (declare (fixnum target-end-pos))
        (loop for curr-pos of-type fixnum from target-end-pos downto start-pos
              thereis (funcall next-fn curr-pos))))))

(defgeneric create-greedy-constant-length-matcher (repetition next-fn)
  (declare #.*standard-optimize-settings*)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is greedy and the minimal number of
repetitions is zero.  It is furthermore assumed that the inner regex
of REPETITION is of fixed length and doesn't contain registers."))

(defmethod create-greedy-constant-length-matcher ((repetition repetition)
                                                  next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((len (len repetition))
        (maximum (maximum repetition))
        (regex (regex repetition))
        (min-rest (min-rest repetition)))
    (declare (fixnum len min-rest)
             (function next-fn))
    (cond ((zerop len)
            ;; inner regex has zero-length, so we can discard it
            ;; completely
            next-fn)
          (t
            ;; now first try to optimize for a couple of common cases
            (typecase regex
              (str
                (let ((str (str regex)))
                  (if (= 1 len)
                    ;; a single character
                    (let ((chr (schar str 0)))
                      (if (case-insensitive-p regex)
                        (greedy-constant-length-closure
                         (char-equal chr (schar *string* curr-pos)))
                        (greedy-constant-length-closure
                         (char= chr (schar *string* curr-pos)))))
                    ;; a string
                    (if (case-insensitive-p regex)
                      (greedy-constant-length-closure
                       (*string*-equal str curr-pos (+ curr-pos len) 0 len))
                      (greedy-constant-length-closure
                       (*string*= str curr-pos (+ curr-pos len) 0 len))))))
              (char-class
                ;; a character class
                (insert-char-class-tester (regex (schar *string* curr-pos))
                  (greedy-constant-length-closure
                   (char-class-test))))
              (everything
                ;; an EVERYTHING object, i.e. a dot
                (if (single-line-p regex)
                  (create-greedy-everything-matcher maximum min-rest next-fn)
                  (greedy-constant-length-closure
                   (char/= #\Newline (schar *string* curr-pos)))))
              (t
                ;; the general case - we build an inner matcher which
                ;; just checks for immediate success, i.e. NEXT-FN is
                ;; #'IDENTITY
                (let ((inner-matcher (create-matcher-aux regex #'identity)))
                  (declare (function inner-matcher))
                  (greedy-constant-length-closure
                   (funcall inner-matcher curr-pos)))))))))

(defgeneric create-greedy-no-zero-matcher (repetition next-fn)
  (declare #.*standard-optimize-settings*)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is greedy and the minimal number of
repetitions is zero.  It is furthermore assumed that the inner regex
of REPETITION can never match a zero-length string \(or instead the
maximal number of repetitions is 1)."))

(defmethod create-greedy-no-zero-matcher ((repetition repetition) next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((maximum (maximum repetition))
        ;; REPEAT-MATCHER is part of the closure's environment but it
        ;; can only be defined after GREEDY-AUX is defined
        repeat-matcher)
    (declare (function next-fn))
    (cond
      ((eql maximum 1)
        ;; this is essentially like the next case but with a known
        ;; MAXIMUM of 1 we can get away without a counter; note that
        ;; we always arrive here if CONVERT optimizes <regex>* to
        ;; (?:<regex'>*<regex>)?
        (setq repeat-matcher
                (create-matcher-aux (regex repetition) next-fn))
        (lambda (start-pos)
          (declare (function repeat-matcher))
          (or (funcall repeat-matcher start-pos)
              (funcall next-fn start-pos))))
      (maximum
        ;; we make a reservation for our slot in *REPEAT-COUNTERS*
        ;; because we need to keep track whether we've reached MAXIMUM
        ;; repetitions
        (let ((rep-num (incf-after *rep-num*)))
          (flet ((greedy-aux (start-pos)
                   (declare (fixnum start-pos maximum rep-num)
                            (function repeat-matcher))
                   ;; the actual matcher which first tries to match the
                   ;; inner regex of REPETITION (if we haven't done so
                   ;; too often) and on failure calls NEXT-FN
                   (or (and (< (aref *repeat-counters* rep-num) maximum)
                            (incf (aref *repeat-counters* rep-num))
                            ;; note that REPEAT-MATCHER will call
                            ;; GREEDY-AUX again recursively
                            (prog1
                              (funcall repeat-matcher start-pos)
                              (decf (aref *repeat-counters* rep-num))))
                       (funcall next-fn start-pos))))
            ;; create a closure to match the inner regex and to
            ;; implement backtracking via GREEDY-AUX
            (setq repeat-matcher
                    (create-matcher-aux (regex repetition) #'greedy-aux))
            ;; the closure we return is just a thin wrapper around
            ;; GREEDY-AUX to initialize the repetition counter
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (setf (aref *repeat-counters* rep-num) 0)
              (greedy-aux start-pos)))))
      (t
        ;; easier code because we're not bounded by MAXIMUM, but
        ;; basically the same
        (flet ((greedy-aux (start-pos)
                 (declare (fixnum start-pos)
                          (function repeat-matcher))
                 (or (funcall repeat-matcher start-pos)
                     (funcall next-fn start-pos))))
          (setq repeat-matcher
                  (create-matcher-aux (regex repetition) #'greedy-aux))
          #'greedy-aux)))))

(defgeneric create-greedy-matcher (repetition next-fn)
  (declare #.*standard-optimize-settings*)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is greedy and the minimal number of
repetitions is zero."))

(defmethod create-greedy-matcher ((repetition repetition) next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((maximum (maximum repetition))
        ;; we make a reservation for our slot in *LAST-POS-STORES* because
        ;; we have to watch out for endless loops as the inner regex might
        ;; match zero-length strings
        (zero-length-num (incf-after *zero-length-num*))
        ;; REPEAT-MATCHER is part of the closure's environment but it
        ;; can only be defined after GREEDY-AUX is defined
        repeat-matcher)
    (declare (fixnum zero-length-num)
             (function next-fn))
    (cond
      (maximum
        ;; we make a reservation for our slot in *REPEAT-COUNTERS*
        ;; because we need to keep track whether we've reached MAXIMUM
        ;; repetitions
        (let ((rep-num (incf-after *rep-num*)))
          (flet ((greedy-aux (start-pos)
                   ;; the actual matcher which first tries to match the
                   ;; inner regex of REPETITION (if we haven't done so
                   ;; too often) and on failure calls NEXT-FN
                   (declare (fixnum start-pos maximum rep-num)
                            (function repeat-matcher))
                   (let ((old-last-pos
                           (svref *last-pos-stores* zero-length-num)))
                     (when (and old-last-pos
                                (= (the fixnum old-last-pos) start-pos))
                       ;; stop immediately if we've been here before,
                       ;; i.e. if the last attempt matched a zero-length
                       ;; string
                       (return-from greedy-aux (funcall next-fn start-pos)))
                     ;; otherwise remember this position for the next
                     ;; repetition
                     (setf (svref *last-pos-stores* zero-length-num) start-pos)
                     (or (and (< (aref *repeat-counters* rep-num) maximum)
                              (incf (aref *repeat-counters* rep-num))
                              ;; note that REPEAT-MATCHER will call
                              ;; GREEDY-AUX again recursively
                              (prog1
                                (funcall repeat-matcher start-pos)
                                (decf (aref *repeat-counters* rep-num))
                                (setf (svref *last-pos-stores* zero-length-num)
                                        old-last-pos)))
                         (funcall next-fn start-pos)))))
            ;; create a closure to match the inner regex and to
            ;; implement backtracking via GREEDY-AUX
            (setq repeat-matcher
                    (create-matcher-aux (regex repetition) #'greedy-aux))
            ;; the closure we return is just a thin wrapper around
            ;; GREEDY-AUX to initialize the repetition counter and our
            ;; slot in *LAST-POS-STORES*
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (setf (aref *repeat-counters* rep-num) 0
                    (svref *last-pos-stores* zero-length-num) nil)
              (greedy-aux start-pos)))))
      (t
        ;; easier code because we're not bounded by MAXIMUM, but
        ;; basically the same
        (flet ((greedy-aux (start-pos)
                 (declare (fixnum start-pos)
                          (function repeat-matcher))
                 (let ((old-last-pos
                         (svref *last-pos-stores* zero-length-num)))
                   (when (and old-last-pos
                              (= (the fixnum old-last-pos) start-pos))
                     (return-from greedy-aux (funcall next-fn start-pos)))
                   (setf (svref *last-pos-stores* zero-length-num) start-pos)
                   (or (prog1
                         (funcall repeat-matcher start-pos)
                         (setf (svref *last-pos-stores* zero-length-num) old-last-pos))
                       (funcall next-fn start-pos)))))
          (setq repeat-matcher
                  (create-matcher-aux (regex repetition) #'greedy-aux))
          (lambda (start-pos)
            (declare (fixnum start-pos))
            (setf (svref *last-pos-stores* zero-length-num) nil)
            (greedy-aux start-pos)))))))
  
;; code for non-greedy repetitions with minimum zero

(defmacro non-greedy-constant-length-closure (check-curr-pos)
  "This is the template for simple non-greedy repetitions \(where
simple means that the minimum number of repetitions is zero, that the
inner regex to be checked is of fixed length LEN, and that it doesn't
contain registers, i.e. there's no need for backtracking).
CHECK-CURR-POS is a form which checks whether the inner regex of the
repetition matches at CURR-POS."
  `(if maximum
    (lambda (start-pos)
      (declare (fixnum start-pos maximum))
      ;; because we know LEN we know in advance where to stop at the
      ;; latest; we also take into consideration MIN-REST, i.e. the
      ;; minimal length of the part behind the repetition
      (let ((target-end-pos (min (1+ (- *end-pos* len min-rest))
                                 (+ start-pos
                                    (the fixnum (* len maximum))))))
        ;; move forward by LEN and always try NEXT-FN first, then
        ;; CHECK-CUR-POS
        (loop for curr-pos of-type fixnum from start-pos
                                          below target-end-pos
                                          by len
              thereis (funcall next-fn curr-pos)
              while ,check-curr-pos
              finally (return (funcall next-fn curr-pos)))))
  ;; basically the same code; it's just a bit easier because we're
  ;; not bounded by MAXIMUM
  (lambda (start-pos)
    (declare (fixnum start-pos))
    (let ((target-end-pos (1+ (- *end-pos* len min-rest))))
      (loop for curr-pos of-type fixnum from start-pos
                                        below target-end-pos
                                        by len
            thereis (funcall next-fn curr-pos)
            while ,check-curr-pos
            finally (return (funcall next-fn curr-pos)))))))

(defgeneric create-non-greedy-constant-length-matcher (repetition next-fn)
  (declare #.*standard-optimize-settings*)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is non-greedy and the minimal number of
repetitions is zero.  It is furthermore assumed that the inner regex
of REPETITION is of fixed length and doesn't contain registers."))

(defmethod create-non-greedy-constant-length-matcher ((repetition repetition) next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((len (len repetition))
        (maximum (maximum repetition))
        (regex (regex repetition))
        (min-rest (min-rest repetition)))
    (declare (fixnum len min-rest)
             (function next-fn))
    (cond ((zerop len)
            ;; inner regex has zero-length, so we can discard it
            ;; completely
            next-fn)
          (t
            ;; now first try to optimize for a couple of common cases
            (typecase regex
              (str
                (let ((str (str regex)))
                  (if (= 1 len)
                    ;; a single character
                    (let ((chr (schar str 0)))
                      (if (case-insensitive-p regex)
                        (non-greedy-constant-length-closure
                         (char-equal chr (schar *string* curr-pos)))
                        (non-greedy-constant-length-closure
                         (char= chr (schar *string* curr-pos)))))
                    ;; a string
                    (if (case-insensitive-p regex)
                      (non-greedy-constant-length-closure
                       (*string*-equal str curr-pos (+ curr-pos len) 0 len))
                      (non-greedy-constant-length-closure
                       (*string*= str curr-pos (+ curr-pos len) 0 len))))))
              (char-class
                ;; a character class
                (insert-char-class-tester (regex (schar *string* curr-pos))
                  (non-greedy-constant-length-closure
                   (char-class-test))))
              (everything
                (if (single-line-p regex)
                  ;; a dot which really can match everything; we rely
                  ;; on the compiler to optimize this away
                  (non-greedy-constant-length-closure
                   t)
                  ;; a dot which has to watch out for #\Newline
                  (non-greedy-constant-length-closure
                   (char/= #\Newline (schar *string* curr-pos)))))
              (t
                ;; the general case - we build an inner matcher which
                ;; just checks for immediate success, i.e. NEXT-FN is
                ;; #'IDENTITY
                (let ((inner-matcher (create-matcher-aux regex #'identity)))
                  (declare (function inner-matcher))
                  (non-greedy-constant-length-closure
                   (funcall inner-matcher curr-pos)))))))))

(defgeneric create-non-greedy-no-zero-matcher (repetition next-fn)
  (declare #.*standard-optimize-settings*)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is non-greedy and the minimal number of
repetitions is zero.  It is furthermore assumed that the inner regex
of REPETITION can never match a zero-length string \(or instead the
maximal number of repetitions is 1)."))

(defmethod create-non-greedy-no-zero-matcher ((repetition repetition) next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((maximum (maximum repetition))
        ;; REPEAT-MATCHER is part of the closure's environment but it
        ;; can only be defined after NON-GREEDY-AUX is defined
        repeat-matcher)
    (declare (function next-fn))
    (cond
      ((eql maximum 1)
        ;; this is essentially like the next case but with a known
        ;; MAXIMUM of 1 we can get away without a counter
        (setq repeat-matcher
                (create-matcher-aux (regex repetition) next-fn))
        (lambda (start-pos)
          (declare (function repeat-matcher))
          (or (funcall next-fn start-pos)
              (funcall repeat-matcher start-pos))))
      (maximum
        ;; we make a reservation for our slot in *REPEAT-COUNTERS*
        ;; because we need to keep track whether we've reached MAXIMUM
        ;; repetitions
        (let ((rep-num (incf-after *rep-num*)))
          (flet ((non-greedy-aux (start-pos)
                   ;; the actual matcher which first calls NEXT-FN and
                   ;; on failure tries to match the inner regex of
                   ;; REPETITION (if we haven't done so too often)
                   (declare (fixnum start-pos maximum rep-num)
                            (function repeat-matcher))
                   (or (funcall next-fn start-pos)
                       (and (< (aref *repeat-counters* rep-num) maximum)
                            (incf (aref *repeat-counters* rep-num))
                            ;; note that REPEAT-MATCHER will call
                            ;; NON-GREEDY-AUX again recursively
                            (prog1
                              (funcall repeat-matcher start-pos)
                              (decf (aref *repeat-counters* rep-num)))))))
            ;; create a closure to match the inner regex and to
            ;; implement backtracking via NON-GREEDY-AUX
            (setq repeat-matcher
                    (create-matcher-aux (regex repetition) #'non-greedy-aux))
            ;; the closure we return is just a thin wrapper around
            ;; NON-GREEDY-AUX to initialize the repetition counter
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (setf (aref *repeat-counters* rep-num) 0)
              (non-greedy-aux start-pos)))))
      (t
        ;; easier code because we're not bounded by MAXIMUM, but
        ;; basically the same
        (flet ((non-greedy-aux (start-pos)
                 (declare (fixnum start-pos)
                          (function repeat-matcher))
                 (or (funcall next-fn start-pos)
                     (funcall repeat-matcher start-pos))))
          (setq repeat-matcher
                  (create-matcher-aux (regex repetition) #'non-greedy-aux))
          #'non-greedy-aux)))))
  
(defgeneric create-non-greedy-matcher (repetition next-fn)
  (declare #.*standard-optimize-settings*)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is non-greedy and the minimal number of
repetitions is zero."))

(defmethod create-non-greedy-matcher ((repetition repetition) next-fn)
  (declare #.*standard-optimize-settings*)
  ;; we make a reservation for our slot in *LAST-POS-STORES* because
  ;; we have to watch out for endless loops as the inner regex might
  ;; match zero-length strings
  (let ((zero-length-num (incf-after *zero-length-num*))
        (maximum (maximum repetition))
        ;; REPEAT-MATCHER is part of the closure's environment but it
        ;; can only be defined after NON-GREEDY-AUX is defined
        repeat-matcher)
    (declare (fixnum zero-length-num)
             (function next-fn))
    (cond
      (maximum
        ;; we make a reservation for our slot in *REPEAT-COUNTERS*
        ;; because we need to keep track whether we've reached MAXIMUM
        ;; repetitions
        (let ((rep-num (incf-after *rep-num*)))
          (flet ((non-greedy-aux (start-pos)
                   ;; the actual matcher which first calls NEXT-FN and
                   ;; on failure tries to match the inner regex of
                   ;; REPETITION (if we haven't done so too often)
                   (declare (fixnum start-pos maximum rep-num)
                            (function repeat-matcher))
                   (let ((old-last-pos
                           (svref *last-pos-stores* zero-length-num)))
                     (when (and old-last-pos
                                (= (the fixnum old-last-pos) start-pos))
                       ;; stop immediately if we've been here before,
                       ;; i.e. if the last attempt matched a zero-length
                       ;; string
                       (return-from non-greedy-aux (funcall next-fn start-pos)))
                     ;; otherwise remember this position for the next
                     ;; repetition
                     (setf (svref *last-pos-stores* zero-length-num) start-pos)
                     (or (funcall next-fn start-pos)
                         (and (< (aref *repeat-counters* rep-num) maximum)
                              (incf (aref *repeat-counters* rep-num))
                              ;; note that REPEAT-MATCHER will call
                              ;; NON-GREEDY-AUX again recursively
                              (prog1
                                (funcall repeat-matcher start-pos)
                                (decf (aref *repeat-counters* rep-num))
                                (setf (svref *last-pos-stores* zero-length-num)
                                        old-last-pos)))))))
            ;; create a closure to match the inner regex and to
            ;; implement backtracking via NON-GREEDY-AUX
            (setq repeat-matcher
                    (create-matcher-aux (regex repetition) #'non-greedy-aux))
            ;; the closure we return is just a thin wrapper around
            ;; NON-GREEDY-AUX to initialize the repetition counter and our
            ;; slot in *LAST-POS-STORES*
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (setf (aref *repeat-counters* rep-num) 0
                    (svref *last-pos-stores* zero-length-num) nil)
              (non-greedy-aux start-pos)))))
      (t
        ;; easier code because we're not bounded by MAXIMUM, but
        ;; basically the same
        (flet ((non-greedy-aux (start-pos)
                 (declare (fixnum start-pos)
                          (function repeat-matcher))
                 (let ((old-last-pos
                         (svref *last-pos-stores* zero-length-num)))
                   (when (and old-last-pos
                              (= (the fixnum old-last-pos) start-pos))
                     (return-from non-greedy-aux (funcall next-fn start-pos)))
                   (setf (svref *last-pos-stores* zero-length-num) start-pos)
                   (or (funcall next-fn start-pos)
                       (prog1
                         (funcall repeat-matcher start-pos)
                         (setf (svref *last-pos-stores* zero-length-num)
                                 old-last-pos))))))
          (setq repeat-matcher
                  (create-matcher-aux (regex repetition) #'non-greedy-aux))
          (lambda (start-pos)
            (declare (fixnum start-pos))
            (setf (svref *last-pos-stores* zero-length-num) nil)
            (non-greedy-aux start-pos)))))))
  
;; code for constant repetitions, i.e. those with a fixed number of repetitions
                      
(defmacro constant-repetition-constant-length-closure (check-curr-pos)
  "This is the template for simple constant repetitions (where simple
means that the inner regex to be checked is of fixed length LEN, and
that it doesn't contain registers, i.e. there's no need for
backtracking) and where constant means that MINIMUM is equal to
MAXIMUM.  CHECK-CURR-POS is a form which checks whether the inner
regex of the repetition matches at CURR-POS."
  `(lambda (start-pos)
    (declare (fixnum start-pos))
      (let ((target-end-pos (+ start-pos
                               (the fixnum (* len repetitions)))))
        (declare (fixnum target-end-pos))
        ;; first check if we won't go beyond the end of the string
        (and (>= *end-pos* target-end-pos)
             ;; then loop through all repetitions step by step
             (loop for curr-pos of-type fixnum from start-pos
                                               below target-end-pos
                                               by len
                   always ,check-curr-pos)
             ;; finally call NEXT-FN if we made it that far
             (funcall next-fn target-end-pos)))))

(defgeneric create-constant-repetition-constant-length-matcher
    (repetition next-fn)
  (declare #.*standard-optimize-settings*)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION has a constant number of repetitions.
It is furthermore assumed that the inner regex of REPETITION is of
fixed length and doesn't contain registers."))

(defmethod create-constant-repetition-constant-length-matcher
       ((repetition repetition) next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((len (len repetition))
        (repetitions (minimum repetition))
        (regex (regex repetition)))
    (declare (fixnum len repetitions)
             (function next-fn))
    (if (zerop len)
      ;; if the length is zero it suffices to try once
      (create-matcher-aux regex next-fn)
      ;; otherwise try to optimize for a couple of common cases
      (typecase regex
        (str
          (let ((str (str regex)))
            (if (= 1 len)
              ;; a single character
              (let ((chr (schar str 0)))
                (if (case-insensitive-p regex)
                  (constant-repetition-constant-length-closure
                   (and (char-equal chr (schar *string* curr-pos))
                        (1+ curr-pos)))
                  (constant-repetition-constant-length-closure
                   (and (char= chr (schar *string* curr-pos))
                        (1+ curr-pos)))))
              ;; a string
              (if (case-insensitive-p regex)
                (constant-repetition-constant-length-closure
                 (let ((next-pos (+ curr-pos len)))
                   (declare (fixnum next-pos))
                   (and (*string*-equal str curr-pos next-pos 0 len)
                        next-pos)))
                (constant-repetition-constant-length-closure
                 (let ((next-pos (+ curr-pos len)))
                   (declare (fixnum next-pos))
                   (and (*string*= str curr-pos next-pos 0 len)
                        next-pos)))))))
        (char-class
          ;; a character class
          (insert-char-class-tester (regex (schar *string* curr-pos))
            (constant-repetition-constant-length-closure
             (and (char-class-test)
                  (1+ curr-pos)))))
        (everything
          (if (single-line-p regex)
            ;; a dot which really matches everything - we just have to
            ;; advance the index into *STRING* accordingly and check
            ;; if we didn't go past the end
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (let ((next-pos (+ start-pos repetitions)))
                (declare (fixnum next-pos))
                (and (<= next-pos *end-pos*)
                     (funcall next-fn next-pos))))
            ;; a dot which is not in single-line-mode - make sure we
            ;; don't match #\Newline
            (constant-repetition-constant-length-closure
             (and (char/= #\Newline (schar *string* curr-pos))
                  (1+ curr-pos)))))
        (t
          ;; the general case - we build an inner matcher which just
          ;; checks for immediate success, i.e. NEXT-FN is #'IDENTITY
          (let ((inner-matcher (create-matcher-aux regex #'identity)))
            (declare (function inner-matcher))
            (constant-repetition-constant-length-closure
             (funcall inner-matcher curr-pos))))))))
  
(defgeneric create-constant-repetition-matcher (repetition next-fn)
  (declare #.*standard-optimize-settings*)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION has a constant number of repetitions."))

(defmethod create-constant-repetition-matcher ((repetition repetition) next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((repetitions (minimum repetition))
        ;; we make a reservation for our slot in *REPEAT-COUNTERS*
        ;; because we need to keep track of the number of repetitions
        (rep-num (incf-after *rep-num*))
        ;; REPEAT-MATCHER is part of the closure's environment but it
        ;; can only be defined after NON-GREEDY-AUX is defined
        repeat-matcher)
    (declare (fixnum repetitions rep-num)
             (function next-fn))
    (if (zerop (min-len repetition))
      ;; we make a reservation for our slot in *LAST-POS-STORES*
      ;; because we have to watch out for needless loops as the inner
      ;; regex might match zero-length strings
      (let ((zero-length-num (incf-after *zero-length-num*)))
        (declare (fixnum zero-length-num))
        (flet ((constant-aux (start-pos)
                 ;; the actual matcher which first calls NEXT-FN and
                 ;; on failure tries to match the inner regex of
                 ;; REPETITION (if we haven't done so too often)
                 (declare (fixnum start-pos)
                          (function repeat-matcher))
                 (let ((old-last-pos
                         (svref *last-pos-stores* zero-length-num)))
                   (when (and old-last-pos
                              (= (the fixnum old-last-pos) start-pos))
                     ;; if we've been here before we matched a
                     ;; zero-length string the last time, so we can
                     ;; just carry on because we will definitely be
                     ;; able to do this again often enough
                     (return-from constant-aux (funcall next-fn start-pos)))
                   ;; otherwise remember this position for the next
                   ;; repetition
                   (setf (svref *last-pos-stores* zero-length-num) start-pos)
                   (cond ((< (aref *repeat-counters* rep-num) repetitions)
                           ;; not enough repetitions yet, try it again
                           (incf (aref *repeat-counters* rep-num))
                           ;; note that REPEAT-MATCHER will call
                           ;; CONSTANT-AUX again recursively
                           (prog1
                             (funcall repeat-matcher start-pos)
                             (decf (aref *repeat-counters* rep-num))
                             (setf (svref *last-pos-stores* zero-length-num)
                                     old-last-pos)))
                         (t
                           ;; we're done - call NEXT-FN
                           (funcall next-fn start-pos))))))
          ;; create a closure to match the inner regex and to
          ;; implement backtracking via CONSTANT-AUX
          (setq repeat-matcher
                  (create-matcher-aux (regex repetition) #'constant-aux))
          ;; the closure we return is just a thin wrapper around
          ;; CONSTANT-AUX to initialize the repetition counter
          (lambda (start-pos)
            (declare (fixnum start-pos))
            (setf (aref *repeat-counters* rep-num) 0
                  (aref *last-pos-stores* zero-length-num) nil)
            (constant-aux start-pos))))
      ;; easier code because we don't have to care about zero-length
      ;; matches but basically the same
      (flet ((constant-aux (start-pos)
               (declare (fixnum start-pos)
                        (function repeat-matcher))
               (cond ((< (aref *repeat-counters* rep-num) repetitions)
                       (incf (aref *repeat-counters* rep-num))
                       (prog1
                         (funcall repeat-matcher start-pos)
                         (decf (aref *repeat-counters* rep-num))))
                     (t (funcall next-fn start-pos)))))
        (setq repeat-matcher
                (create-matcher-aux (regex repetition) #'constant-aux))
        (lambda (start-pos)
          (declare (fixnum start-pos))
          (setf (aref *repeat-counters* rep-num) 0)
          (constant-aux start-pos))))))
  
;; the actual CREATE-MATCHER-AUX method for REPETITION objects which
;; utilizes all the functions and macros defined above

(defmethod create-matcher-aux ((repetition repetition) next-fn)
  (declare #.*standard-optimize-settings*)
  (with-slots (minimum maximum len min-len greedyp contains-register-p)
      repetition
    (cond ((and maximum
                (zerop maximum))
           ;; this should have been optimized away by CONVERT but just
           ;; in case...
           (error "Got REPETITION with MAXIMUM 0 \(should not happen)"))
          ((and maximum
                (= minimum maximum 1))
           ;; this should have been optimized away by CONVERT but just
           ;; in case...
           (error "Got REPETITION with MAXIMUM 1 and MINIMUM 1 \(should not happen)"))
          ((and (eql minimum maximum)
                len
                (not contains-register-p))
           (create-constant-repetition-constant-length-matcher repetition next-fn))
          ((eql minimum maximum)
           (create-constant-repetition-matcher repetition next-fn))
          ((and greedyp
                len
                (not contains-register-p))
           (create-greedy-constant-length-matcher repetition next-fn))
          ((and greedyp
                (or (plusp min-len)
                    (eql maximum 1)))
           (create-greedy-no-zero-matcher repetition next-fn))
          (greedyp
           (create-greedy-matcher repetition next-fn))
          ((and len
                (plusp len)
                (not contains-register-p))
           (create-non-greedy-constant-length-matcher repetition next-fn))
          ((or (plusp min-len)
               (eql maximum 1))
           (create-non-greedy-no-zero-matcher repetition next-fn))
          (t
           (create-non-greedy-matcher repetition next-fn)))))
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/scanner.lisp,v 1.36 2009/09/17 19:17:31 edi Exp $

;;; Here the scanner for the actual regex as well as utility scanners
;;; for the constant start and end strings are created.

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

(defmacro bmh-matcher-aux (&key case-insensitive-p)
  "Auxiliary macro used by CREATE-BMH-MATCHER."
  (let ((char-compare (if case-insensitive-p 'char-equal 'char=)))
    `(lambda (start-pos)
       (declare (fixnum start-pos))
       (if (or (minusp start-pos)
               (> (the fixnum (+ start-pos m)) *end-pos*))
         nil
         (loop named bmh-matcher
               for k of-type fixnum = (+ start-pos m -1)
               then (+ k (max 1 (aref skip (char-code (schar *string* k)))))
               while (< k *end-pos*)
               do (loop for j of-type fixnum downfrom (1- m)
                        for i of-type fixnum downfrom k
                        while (and (>= j 0)
                                   (,char-compare (schar *string* i)
                                                  (schar pattern j)))
                        finally (if (minusp j)
                                  (return-from bmh-matcher (1+ i)))))))))

(defun create-bmh-matcher (pattern case-insensitive-p)
  "Returns a Boyer-Moore-Horspool matcher which searches the (special)
simple-string *STRING* for the first occurence of the substring
PATTERN.  The search starts at the position START-POS within *STRING*
and stops before *END-POS* is reached.  Depending on the second
argument the search is case-insensitive or not.  If the special
variable *USE-BMH-MATCHERS* is NIL, use the standard SEARCH function
instead.  \(BMH matchers are faster but need much more space.)"
  (declare #.*standard-optimize-settings*)
  ;; see <http://www-igm.univ-mlv.fr/~lecroq/string/node18.html> for
  ;; details
  (unless *use-bmh-matchers*
    (let ((test (if case-insensitive-p #'char-equal #'char=)))
      (return-from create-bmh-matcher
        (lambda (start-pos)
          (declare (fixnum start-pos))
          (and (not (minusp start-pos))
               (search pattern
                       *string*
                       :start2 start-pos
                       :end2 *end-pos*
                       :test test))))))
  (let* ((m (length pattern))
	 (skip (make-array *regex-char-code-limit*
                           :element-type 'fixnum
                           :initial-element m)))
    (declare (fixnum m))
    (loop for k of-type fixnum below m
          if case-insensitive-p
          do (setf (aref skip (char-code (char-upcase (schar pattern k)))) (- m k 1)
                   (aref skip (char-code (char-downcase (schar pattern k)))) (- m k 1))
	  else
          do (setf (aref skip (char-code (schar pattern k))) (- m k 1)))
    (if case-insensitive-p
      (bmh-matcher-aux :case-insensitive-p t)
      (bmh-matcher-aux))))

(defmacro char-searcher-aux (&key case-insensitive-p)
  "Auxiliary macro used by CREATE-CHAR-SEARCHER."
  (let ((char-compare (if case-insensitive-p 'char-equal 'char=)))
    `(lambda (start-pos)
      (declare (fixnum start-pos))
      (and (not (minusp start-pos))
           (loop for i of-type fixnum from start-pos below *end-pos*
                 thereis (and (,char-compare (schar *string* i) chr) i))))))

(defun create-char-searcher (chr case-insensitive-p)
  "Returns a function which searches the (special) simple-string
*STRING* for the first occurence of the character CHR. The search
starts at the position START-POS within *STRING* and stops before
*END-POS* is reached.  Depending on the second argument the search is
case-insensitive or not."
  (declare #.*standard-optimize-settings*)
  (if case-insensitive-p
    (char-searcher-aux :case-insensitive-p t)
    (char-searcher-aux)))

(declaim (inline newline-skipper))
(defun newline-skipper (start-pos)
  "Finds the next occurence of a character in *STRING* which is behind
a #\Newline."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum start-pos))
  ;; we can start with (1- START-POS) without testing for (PLUSP
  ;; START-POS) because we know we'll never call NEWLINE-SKIPPER on
  ;; the first iteration
  (loop for i of-type fixnum from (1- start-pos) below *end-pos*
        thereis (and (char= (schar *string* i)
                            #\Newline)
                     (1+ i))))

(defmacro insert-advance-fn (advance-fn)
  "Creates the actual closure returned by CREATE-SCANNER-AUX by
replacing '(ADVANCE-FN-DEFINITION) with a suitable definition for
ADVANCE-FN.  This is a utility macro used by CREATE-SCANNER-AUX."
  (subst
   advance-fn '(advance-fn-definition)
   '(lambda (string start end)
     (block scan
       ;; initialize a couple of special variables used by the
       ;; matchers (see file specials.lisp)
       (let* ((*string* string)
              (*start-pos* start)
              (*end-pos* end)
              ;; we will search forward for END-STRING if this value
              ;; isn't at least as big as POS (see ADVANCE-FN), so it
              ;; is safe to start to the left of *START-POS*; note
              ;; that this value will _never_ be decremented - this
              ;; is crucial to the scanning process
              (*end-string-pos* (1- *start-pos*))
              ;; the next five will shadow the variables defined by
              ;; DEFPARAMETER; at this point, we don't know if we'll
              ;; actually use them, though
              (*repeat-counters* *repeat-counters*)
              (*last-pos-stores* *last-pos-stores*)
              (*reg-starts* *reg-starts*)
              (*regs-maybe-start* *regs-maybe-start*)
              (*reg-ends* *reg-ends*)
              ;; we might be able to optimize the scanning process by
              ;; (virtually) shifting *START-POS* to the right
              (scan-start-pos *start-pos*)
              (starts-with-str (if start-string-test
                                 (str starts-with)
                                 nil))
              ;; we don't need to try further than MAX-END-POS
              (max-end-pos (- *end-pos* min-len)))
         (declare (fixnum scan-start-pos)
                  (function match-fn))
         ;; definition of ADVANCE-FN will be inserted here by macrology
         (labels ((advance-fn-definition))
           (declare (inline advance-fn))
           (when (plusp rep-num)
             ;; we have at least one REPETITION which needs to count
             ;; the number of repetitions
             (setq *repeat-counters* (make-array rep-num
                                                 :initial-element 0
                                                 :element-type 'fixnum)))
           (when (plusp zero-length-num)
             ;; we have at least one REPETITION which needs to watch
             ;; out for zero-length repetitions
             (setq *last-pos-stores* (make-array zero-length-num
                                                 :initial-element nil)))
           (when (plusp reg-num)
             ;; we have registers in our regular expression
             (setq *reg-starts* (make-array reg-num :initial-element nil)
                   *regs-maybe-start* (make-array reg-num :initial-element nil)
                   *reg-ends* (make-array reg-num :initial-element nil)))
           (when end-anchored-p
             ;; the regular expression has a constant end string which
             ;; is anchored at the very end of the target string
             ;; (perhaps modulo a #\Newline)
             (let ((end-test-pos (- *end-pos* (the fixnum end-string-len))))
               (declare (fixnum end-test-pos)
                        (function end-string-test))
               (unless (setq *end-string-pos* (funcall end-string-test
                                                       end-test-pos))
                 (when (and (= 1 (the fixnum end-anchored-p))
                            (> *end-pos* scan-start-pos)
                            (char= #\Newline (schar *string* (1- *end-pos*))))
                   ;; if we didn't find an end string candidate from
                   ;; END-TEST-POS and if a #\Newline at the end is
                   ;; allowed we try it again from one position to the
                   ;; left
                   (setq *end-string-pos* (funcall end-string-test
                                                   (1- end-test-pos))))))
             (unless (and *end-string-pos*
                          (<= *start-pos* *end-string-pos*))
               ;; no end string candidate found, so give up
               (return-from scan nil))
             (when end-string-offset
               ;; if the offset of the constant end string from the
               ;; left of the regular expression is known we can start
               ;; scanning further to the right; this is similar to
               ;; what we might do in ADVANCE-FN
               (setq scan-start-pos (max scan-start-pos
                                         (- (the fixnum *end-string-pos*)
                                            (the fixnum end-string-offset))))))
             (cond
               (start-anchored-p
                 ;; we're anchored at the start of the target string,
                 ;; so no need to try again after first failure
                 (when (or (/= *start-pos* scan-start-pos)
                           (< max-end-pos *start-pos*))
                   ;; if END-STRING-OFFSET has proven that we don't
                   ;; need to bother to scan from *START-POS* or if the
                   ;; minimal length of the regular expression is
                   ;; longer than the target string we give up
                   (return-from scan nil))
                 (when starts-with-str
                   (locally
                     (declare (fixnum starts-with-len))
                     (cond ((and (case-insensitive-p starts-with)
                                 (not (*string*-equal starts-with-str
                                                      *start-pos*
                                                      (+ *start-pos*
                                                         starts-with-len)
                                                      0 starts-with-len)))
                             ;; the regular expression has a
                             ;; case-insensitive constant start string
                             ;; and we didn't find it
                             (return-from scan nil))
                           ((and (not (case-insensitive-p starts-with))
                                 (not (*string*= starts-with-str
                                            *start-pos*
                                            (+ *start-pos* starts-with-len)
                                            0 starts-with-len)))
                             ;; the regular expression has a
                             ;; case-sensitive constant start string
                             ;; and we didn't find it
                             (return-from scan nil))
                           (t nil))))
                 (when (and end-string-test
                            (not end-anchored-p))
                   ;; the regular expression has a constant end string
                   ;; which isn't anchored so we didn't check for it
                   ;; already
                   (block end-string-loop
                     ;; we temporarily use *END-STRING-POS* as our
                     ;; starting position to look for end string
                     ;; candidates
                     (setq *end-string-pos* *start-pos*)
                     (loop
                       (unless (setq *end-string-pos*
                                       (funcall (the function end-string-test)
                                                *end-string-pos*))
                         ;; no end string candidate found, so give up
                         (return-from scan nil))
                       (unless end-string-offset
                         ;; end string doesn't have an offset so we
                         ;; can start scanning now
                         (return-from end-string-loop))
                       (let ((maybe-start-pos (- (the fixnum *end-string-pos*)
                                                 (the fixnum end-string-offset))))
                         (cond ((= maybe-start-pos *start-pos*)
                                 ;; offset of end string into regular
                                 ;; expression matches start anchor -
                                 ;; fine...
                                 (return-from end-string-loop))
                               ((and (< maybe-start-pos *start-pos*)
                                     (< (+ *end-string-pos* end-string-len) *end-pos*))
                                 ;; no match but maybe we find another
                                 ;; one to the right - try again
                                 (incf *end-string-pos*))
                               (t
                                 ;; otherwise give up
                                 (return-from scan nil)))))))
                 ;; if we got here we scan exactly once
                 (let ((next-pos (funcall match-fn *start-pos*)))
                   (when next-pos
                     (values (if next-pos *start-pos* nil)
                             next-pos
                             *reg-starts*
                             *reg-ends*))))
               (t
                 (loop for pos = (if starts-with-everything
                                   ;; don't jump to the next
                                   ;; #\Newline on the first
                                   ;; iteration
                                   scan-start-pos
                                   (advance-fn scan-start-pos))
                           then (advance-fn pos)
                       ;; give up if the regular expression can't fit
                       ;; into the rest of the target string
                       while (and pos
                                  (<= (the fixnum pos) max-end-pos))
                       do (let ((next-pos (funcall match-fn pos)))
                            (when next-pos
                              (return-from scan (values pos
                                                        next-pos
                                                        *reg-starts*
                                                        *reg-ends*)))
                            ;; not yet found, increment POS
                            #-cormanlisp (incf (the fixnum pos))
                            #+cormanlisp (incf pos)))))))))
    :test #'equalp))

(defun create-scanner-aux (match-fn
                           min-len
                           start-anchored-p
                           starts-with
                           start-string-test
                           end-anchored-p
                           end-string-test
                           end-string-len
                           end-string-offset
                           rep-num
                           zero-length-num
                           reg-num)
  "Auxiliary function to create and return a scanner \(which is
actually a closure).  Used by CREATE-SCANNER."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum min-len zero-length-num rep-num reg-num))
  (let ((starts-with-len (if (typep starts-with 'str)
                           (len starts-with)))
        (starts-with-everything (typep starts-with 'everything)))
    (cond
      ;; this COND statement dispatches on the different versions we
      ;; have for ADVANCE-FN and creates different closures for each;
      ;; note that you see only the bodies of ADVANCE-FN below - the
      ;; actual scanner is defined in INSERT-ADVANCE-FN above; (we
      ;; could have done this with closures instead of macrology but
      ;; would have consed a lot more)
      ((and start-string-test end-string-test end-string-offset)
        ;; we know that the regular expression has constant start and
        ;; end strings and we know the end string's offset (from the
        ;; left)
        (insert-advance-fn
          (advance-fn (pos)
            (declare (fixnum end-string-offset starts-with-len)
                     (function start-string-test end-string-test))
            (loop
              (unless (setq pos (funcall start-string-test pos))
                ;; give up completely if we can't find a start string
                ;; candidate
                (return-from scan nil))
              (locally
                ;; from here we know that POS is a FIXNUM
                (declare (fixnum pos))
                (when (= pos (- (the fixnum *end-string-pos*) end-string-offset))
                  ;; if we already found an end string candidate the
                  ;; position of which matches the start string
                  ;; candidate we're done
                  (return-from advance-fn pos))
                (let ((try-pos (+ pos starts-with-len)))
                  ;; otherwise try (again) to find an end string
                  ;; candidate which starts behind the start string
                  ;; candidate
                  (loop
                    (unless (setq *end-string-pos*
                                    (funcall end-string-test try-pos))
                      ;; no end string candidate found, so give up
                      (return-from scan nil))
                    ;; NEW-POS is where we should start scanning
                    ;; according to the end string candidate
                    (let ((new-pos (- (the fixnum *end-string-pos*)
                                      end-string-offset)))
                      (declare (fixnum new-pos *end-string-pos*))
                      (cond ((= new-pos pos)
                              ;; if POS and NEW-POS are equal then the
                              ;; two candidates agree so we're fine
                              (return-from advance-fn pos))
                            ((> new-pos pos)
                              ;; if NEW-POS is further to the right we
                              ;; advance POS and try again, i.e. we go
                              ;; back to the start of the outer LOOP
                              (setq pos new-pos)
                              ;; this means "return from inner LOOP"
                              (return))
                            (t
                              ;; otherwise NEW-POS is smaller than POS,
                              ;; so we have to redo the inner LOOP to
                              ;; find another end string candidate
                              ;; further to the right
                              (setq try-pos (1+ *end-string-pos*))))))))))))
      ((and starts-with-everything end-string-test end-string-offset)
        ;; we know that the regular expression starts with ".*" (which
        ;; is not in single-line-mode, see CREATE-SCANNER-AUX) and ends
        ;; with a constant end string and we know the end string's
        ;; offset (from the left)
        (insert-advance-fn
          (advance-fn (pos)
            (declare (fixnum end-string-offset)
                     (function end-string-test))
            (loop
              (unless (setq pos (newline-skipper pos))
                ;; if we can't find a #\Newline we give up immediately
                (return-from scan nil))
              (locally
                ;; from here we know that POS is a FIXNUM
                (declare (fixnum pos))
                (when (= pos (- (the fixnum *end-string-pos*) end-string-offset))
                  ;; if we already found an end string candidate the
                  ;; position of which matches the place behind the
                  ;; #\Newline we're done
                  (return-from advance-fn pos))
                (let ((try-pos pos))
                  ;; otherwise try (again) to find an end string
                  ;; candidate which starts behind the #\Newline
                  (loop
                    (unless (setq *end-string-pos*
                                    (funcall end-string-test try-pos))
                      ;; no end string candidate found, so we give up
                      (return-from scan nil))
                    ;; NEW-POS is where we should start scanning
                    ;; according to the end string candidate
                    (let ((new-pos (- (the fixnum *end-string-pos*)
                                      end-string-offset)))
                      (declare (fixnum new-pos *end-string-pos*))
                      (cond ((= new-pos pos)
                              ;; if POS and NEW-POS are equal then the
                              ;; the end string candidate agrees with
                              ;; the #\Newline so we're fine
                              (return-from advance-fn pos))
                            ((> new-pos pos)
                              ;; if NEW-POS is further to the right we
                              ;; advance POS and try again, i.e. we go
                              ;; back to the start of the outer LOOP
                              (setq pos new-pos)
                              ;; this means "return from inner LOOP"
                              (return))
                            (t
                              ;; otherwise NEW-POS is smaller than POS,
                              ;; so we have to redo the inner LOOP to
                              ;; find another end string candidate
                              ;; further to the right
                              (setq try-pos (1+ *end-string-pos*))))))))))))
      ((and start-string-test end-string-test)
        ;; we know that the regular expression has constant start and
        ;; end strings; similar to the first case but we only need to
        ;; check for the end string, it doesn't provide enough
        ;; information to advance POS
        (insert-advance-fn
          (advance-fn (pos)
            (declare (function start-string-test end-string-test))
            (unless (setq pos (funcall start-string-test pos))
              (return-from scan nil))
            (if (<= (the fixnum pos)
                    (the fixnum *end-string-pos*))
              (return-from advance-fn pos))
            (unless (setq *end-string-pos* (funcall end-string-test pos))
              (return-from scan nil))
            pos)))
      ((and starts-with-everything end-string-test)
        ;; we know that the regular expression starts with ".*" (which
        ;; is not in single-line-mode, see CREATE-SCANNER-AUX) and ends
        ;; with a constant end string; similar to the second case but we
        ;; only need to check for the end string, it doesn't provide
        ;; enough information to advance POS
        (insert-advance-fn
          (advance-fn (pos)
            (declare (function end-string-test))
            (unless (setq pos (newline-skipper pos))
              (return-from scan nil))
            (if (<= (the fixnum pos)
                    (the fixnum *end-string-pos*))
              (return-from advance-fn pos))
            (unless (setq *end-string-pos* (funcall end-string-test pos))
              (return-from scan nil))
            pos)))
      (start-string-test
        ;; just check for constant start string candidate
        (insert-advance-fn
          (advance-fn (pos)
            (declare (function start-string-test))
            (unless (setq pos (funcall start-string-test pos))
              (return-from scan nil))
            pos)))
      (starts-with-everything
        ;; just advance POS with NEWLINE-SKIPPER
        (insert-advance-fn
          (advance-fn (pos)
            (unless (setq pos (newline-skipper pos))
              (return-from scan nil))
            pos)))
      (end-string-test
        ;; just check for the next end string candidate if POS has
        ;; advanced beyond the last one
        (insert-advance-fn
          (advance-fn (pos)
            (declare (function end-string-test))
            (if (<= (the fixnum pos)
                    (the fixnum *end-string-pos*))
              (return-from advance-fn pos))
            (unless (setq *end-string-pos* (funcall end-string-test pos))
              (return-from scan nil))
            pos)))
      (t
        ;; not enough optimization information about the regular
        ;; expression to optimize so we just return POS
        (insert-advance-fn
          (advance-fn (pos)
            pos))))))
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/api.lisp,v 1.85 2009/09/17 19:17:30 edi Exp $

;;; The external API for creating and using scanners.

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-ppcre)

(defgeneric create-scanner (regex &key case-insensitive-mode
                                       multi-line-mode
                                       single-line-mode
                                       extended-mode
                                       destructive)
  (:documentation "Accepts a regular expression - either as a
parse-tree or as a string - and returns a scan closure which will scan
strings for this regular expression and a list mapping registers to
their names \(NIL stands for unnamed ones).  The \"mode\" keyword
arguments are equivalent to the imsx modifiers in Perl.  If
DESTRUCTIVE is not NIL, the function is allowed to destructively
modify its first argument \(but only if it's a parse tree)."))

#-:use-acl-regexp2-engine
(defmethod create-scanner ((regex-string string) &key case-insensitive-mode
                                                      multi-line-mode
                                                      single-line-mode
                                                      extended-mode
                                                      destructive)
  (declare #.*standard-optimize-settings*)
  (declare (ignore destructive))
  ;; parse the string into a parse-tree and then call CREATE-SCANNER
  ;; again
  (let* ((*extended-mode-p* extended-mode)
         (quoted-regex-string (if *allow-quoting*
                                (quote-sections (clean-comments regex-string extended-mode))
                                regex-string))
         (*syntax-error-string* (copy-seq quoted-regex-string)))
    ;; wrap the result with :GROUP to avoid infinite loops for
    ;; constant strings
    (create-scanner (cons :group (list (parse-string quoted-regex-string)))
                    :case-insensitive-mode case-insensitive-mode
                    :multi-line-mode multi-line-mode
                    :single-line-mode single-line-mode
                    :destructive t)))

#-:use-acl-regexp2-engine
(defmethod create-scanner ((scanner function) &key case-insensitive-mode
                                                   multi-line-mode
                                                   single-line-mode
                                                   extended-mode
                                                   destructive)
  (declare #.*standard-optimize-settings*)
  (declare (ignore destructive))
  (when (or case-insensitive-mode multi-line-mode single-line-mode extended-mode)
    (signal-invocation-error "You can't use the keyword arguments to modify an existing scanner."))
  scanner)

#-:use-acl-regexp2-engine
(defmethod create-scanner ((parse-tree t) &key case-insensitive-mode
                                               multi-line-mode
                                               single-line-mode
                                               extended-mode
                                               destructive)
  (declare #.*standard-optimize-settings*)
  (when extended-mode
    (signal-invocation-error "Extended mode doesn't make sense in parse trees."))
  ;; convert parse-tree into internal representation REGEX and at the
  ;; same time compute the number of registers and the constant string
  ;; (or anchor) the regex starts with (if any)
  (unless destructive
    (setq parse-tree (copy-tree parse-tree)))
  (let (flags)
    (if single-line-mode
      (push :single-line-mode-p flags))
    (if multi-line-mode
      (push :multi-line-mode-p flags))
    (if case-insensitive-mode
      (push :case-insensitive-p flags))
    (when flags
      (setq parse-tree (list :group (cons :flags flags) parse-tree))))
  (let ((*syntax-error-string* nil))
    (multiple-value-bind (regex reg-num starts-with reg-names)
        (convert parse-tree)
      ;; simplify REGEX by flattening nested SEQ and ALTERNATION
      ;; constructs and gathering STR objects
      (let ((regex (gather-strings (flatten regex))))
        ;; set the MIN-REST slots of the REPETITION objects
        (compute-min-rest regex 0)
        ;; set the OFFSET slots of the STR objects
        (compute-offsets regex 0)
        (let* (end-string-offset
               end-anchored-p
               ;; compute the constant string the regex ends with (if
               ;; any) and at the same time set the special variables
               ;; END-STRING-OFFSET and END-ANCHORED-P
               (end-string (end-string regex))
               ;; if we found a non-zero-length end-string we create an
               ;; efficient search function for it
               (end-string-test (and end-string
                                     (plusp (len end-string))
                                     (if (= 1 (len end-string))
                                       (create-char-searcher
                                        (schar (str end-string) 0)
                                        (case-insensitive-p end-string))
                                       (create-bmh-matcher
                                        (str end-string)
                                        (case-insensitive-p end-string)))))
               ;; initialize the counters for CREATE-MATCHER-AUX
               (*rep-num* 0)
               (*zero-length-num* 0)
               ;; create the actual matcher function (which does all the
               ;; work of matching the regular expression) corresponding
               ;; to REGEX and at the same time set the special
               ;; variables *REP-NUM* and *ZERO-LENGTH-NUM*
               (match-fn (create-matcher-aux regex #'identity))
               ;; if the regex starts with a string we create an
               ;; efficient search function for it
               (start-string-test (and (typep starts-with 'str)
                                       (plusp (len starts-with))
                                       (if (= 1 (len starts-with))
                                         (create-char-searcher
                                          (schar (str starts-with) 0)
                                          (case-insensitive-p starts-with))
                                         (create-bmh-matcher
                                          (str starts-with)
                                          (case-insensitive-p starts-with))))))
          (declare (special end-string-offset end-anchored-p end-string))
          ;; now create the scanner and return it
          (values (create-scanner-aux match-fn
                                      (regex-min-length regex)
                                      (or (start-anchored-p regex)
                                          ;; a dot in single-line-mode also
                                          ;; implicitly anchors the regex at
                                          ;; the start, i.e. if we can't match
                                          ;; from the first position we won't
                                          ;; match at all
                                          (and (typep starts-with 'everything)
                                               (single-line-p starts-with)))
                                      starts-with
                                      start-string-test
                                      ;; only mark regex as end-anchored if we
                                      ;; found a non-zero-length string before
                                      ;; the anchor
                                      (and end-string-test end-anchored-p)
                                      end-string-test
                                      (if end-string-test
                                          (len end-string)
                                          nil)
                                      end-string-offset
                                      *rep-num*
                                      *zero-length-num*
                                      reg-num)
                  reg-names))))))

#+:use-acl-regexp2-engine
(declaim (inline create-scanner))
#+:use-acl-regexp2-engine
(defmethod create-scanner ((scanner regexp::regular-expression) &key case-insensitive-mode
                                                                     multi-line-mode
                                                                     single-line-mode
                                                                     extended-mode
                                                                     destructive)
  (declare #.*standard-optimize-settings*)
  (declare (ignore destructive))
  (when (or case-insensitive-mode multi-line-mode single-line-mode extended-mode)
    (signal-invocation-error "You can't use the keyword arguments to modify an existing scanner."))
  scanner)

#+:use-acl-regexp2-engine
(defmethod create-scanner ((parse-tree t) &key case-insensitive-mode
                                               multi-line-mode
                                               single-line-mode
                                               extended-mode
                                               destructive)
  (declare #.*standard-optimize-settings*)
  (declare (ignore destructive))
  (excl:compile-re parse-tree
                   :case-fold case-insensitive-mode
                   :ignore-whitespace extended-mode
                   :multiple-lines multi-line-mode
                   :single-line single-line-mode
                   :return :index))

(defgeneric scan (regex target-string &key start end real-start-pos)
  (:documentation "Searches TARGET-STRING from START to END and tries
to match REGEX.  On success returns four values - the start of the
match, the end of the match, and two arrays denoting the beginnings
and ends of register matches.  On failure returns NIL.  REGEX can be a
string which will be parsed according to Perl syntax, a parse tree, or
a pre-compiled scanner created by CREATE-SCANNER.  TARGET-STRING will
be coerced to a simple string if it isn't one already.  The
REAL-START-POS parameter should be ignored - it exists only for
internal purposes."))

#-:use-acl-regexp2-engine
(defmethod scan ((regex-string string) target-string
                                       &key (start 0)
                                            (end (length target-string))
                                            ((:real-start-pos *real-start-pos*) nil))
  (declare #.*standard-optimize-settings*)
  ;; note that the scanners are optimized for simple strings so we
  ;; have to coerce TARGET-STRING into one if it isn't already
  (funcall (create-scanner regex-string)
           (maybe-coerce-to-simple-string target-string)
           start end))

#-:use-acl-regexp2-engine
(defmethod scan ((scanner function) target-string
                                    &key (start 0)
                                         (end (length target-string))
                                         ((:real-start-pos *real-start-pos*) nil))
  (declare #.*standard-optimize-settings*)
  (funcall scanner
           (maybe-coerce-to-simple-string target-string)
           start end))

#-:use-acl-regexp2-engine
(defmethod scan ((parse-tree t) target-string
                                &key (start 0)
                                     (end (length target-string))
                                     ((:real-start-pos *real-start-pos*) nil))
  (declare #.*standard-optimize-settings*)
  (funcall (create-scanner parse-tree)
           (maybe-coerce-to-simple-string target-string)
           start end))

#+:use-acl-regexp2-engine
(declaim (inline scan))
#+:use-acl-regexp2-engine
(defmethod scan ((parse-tree t) target-string
                                &key (start 0)
                                     (end (length target-string))
                                     ((:real-start-pos *real-start-pos*) nil))
  (declare #.*standard-optimize-settings*)
  (when (< end start)
    (return-from scan nil))
  (let ((results (multiple-value-list (excl:match-re parse-tree target-string
                                                     :start start
                                                     :end end
                                                     :return :index))))
    (declare (dynamic-extent results))
    (cond ((null (first results)) nil)
          (t (let* ((no-of-regs (- (length results) 2))
                    (reg-starts (make-array no-of-regs
                                            :element-type '(or null fixnum)))
                    (reg-ends (make-array no-of-regs
                                          :element-type '(or null fixnum)))
                    (match (second results)))
               (loop for (start . end) in (cddr results)
                     for i from 0
                     do (setf (aref reg-starts i) start
                              (aref reg-ends i) end))
               (values (car match) (cdr match) reg-starts reg-ends))))))

#-:cormanlisp
(define-compiler-macro scan (&whole form &environment env regex target-string &rest rest)
  "Make sure that constant forms are compiled into scanners at compile time."
  (cond ((constantp regex env)
          `(scan (load-time-value (create-scanner ,regex))
                 ,target-string ,@rest))
        (t form)))

(defun scan-to-strings (regex target-string &key (start 0)
                                                 (end (length target-string))
                                                 sharedp)
  "Like SCAN but returns substrings of TARGET-STRING instead of
positions, i.e. this function returns two values on success: the whole
match as a string plus an array of substrings (or NILs) corresponding
to the matched registers.  If SHAREDP is true, the substrings may
share structure with TARGET-STRING."
  (declare #.*standard-optimize-settings*)
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (scan regex target-string :start start :end end)
    (unless match-start
      (return-from scan-to-strings nil))
    (let ((substr-fn (if sharedp #'nsubseq #'subseq)))
      (values (funcall substr-fn
                       target-string match-start match-end)
              (map 'vector
                   (lambda (reg-start reg-end)
                     (if reg-start
                       (funcall substr-fn
                                target-string reg-start reg-end)
                       nil))
                   reg-starts
                   reg-ends)))))

#-:cormanlisp
(define-compiler-macro scan-to-strings
    (&whole form &environment env regex target-string &rest rest)
  "Make sure that constant forms are compiled into scanners at compile time."
  (cond ((constantp regex env)
          `(scan-to-strings (load-time-value (create-scanner ,regex))
                            ,target-string ,@rest))
        (t form)))

(defmacro register-groups-bind (var-list (regex target-string
                                                &key start end sharedp)
                                &body body)
  "Executes BODY with the variables in VAR-LIST bound to the
corresponding register groups after TARGET-STRING has been matched
against REGEX, i.e. each variable is either bound to a string or to
NIL.  If there is no match, BODY is _not_ executed. For each element
of VAR-LIST which is NIL there's no binding to the corresponding
register group.  The number of variables in VAR-LIST must not be
greater than the number of register groups.  If SHAREDP is true, the
substrings may share structure with TARGET-STRING."
  (with-rebinding (target-string)
    (with-unique-names (match-start match-end reg-starts reg-ends
                                    start-index substr-fn)
      (let ((var-bindings
              (loop for (function var) in (normalize-var-list var-list)
                    for counter from 0
                    when var
                      collect `(,var (let ((,start-index
                                             (aref ,reg-starts ,counter)))
                                       (if ,start-index
                                           (funcall ,function
                                                    (funcall ,substr-fn
                                                             ,target-string
                                                             ,start-index
                                                             (aref ,reg-ends ,counter)))
                                           nil))))))
        `(multiple-value-bind (,match-start ,match-end ,reg-starts ,reg-ends)
             (scan ,regex ,target-string :start (or ,start 0)
                                         :end (or ,end (length ,target-string)))
           (declare (ignore ,match-end))
           ,@(unless var-bindings
               `((declare (ignore ,reg-starts ,reg-ends))))
           (when ,match-start
             ,@(if var-bindings
                   `((let* ,(list*
                             `(,substr-fn (if ,sharedp #'nsubseq #'subseq))
                             var-bindings)
                       ,@body))
                   body)))))))

(defmacro do-scans ((match-start match-end reg-starts reg-ends regex
                                 target-string
                                 &optional result-form
                                 &key start end)
                    &body body
                    &environment env)
  "Iterates over TARGET-STRING and tries to match REGEX as often as
possible evaluating BODY with MATCH-START, MATCH-END, REG-STARTS, and
REG-ENDS bound to the four return values of each match in turn.  After
the last match, returns RESULT-FORM if provided or NIL otherwise. An
implicit block named NIL surrounds DO-SCANS; RETURN may be used to
terminate the loop immediately.  If REGEX matches an empty string the
scan is continued one position behind this match. BODY may start with
declarations."
  (with-rebinding (target-string)
    (with-unique-names (%start %end %regex scanner)
      (declare (ignorable %regex scanner))
      ;; the NIL BLOCK to enable exits via (RETURN ...)
      `(block nil
         (let* ((,%start (or ,start 0))
                (,%end (or ,end (length ,target-string)))
                ,@(unless (constantp regex env)
                    ;; leave constant regular expressions as they are -
                    ;; SCAN's compiler macro will take care of them;
                    ;; otherwise create a scanner unless the regex is
                    ;; already a function (otherwise SCAN will do this
                    ;; on each iteration)
                    `((,%regex ,regex)
                      (,scanner (typecase ,%regex
                                  (function ,%regex)
                                  (t (create-scanner ,%regex)))))))
           ;; coerce TARGET-STRING to a simple string unless it is one
           ;; already (otherwise SCAN will do this on each iteration)
           (setq ,target-string
                 (maybe-coerce-to-simple-string ,target-string))
           (loop
            ;; invoke SCAN and bind the returned values to the
            ;; provided variables
            (multiple-value-bind
                (,match-start ,match-end ,reg-starts ,reg-ends)
                (scan ,(cond ((constantp regex env) regex)
                             (t scanner))
                      ,target-string :start ,%start :end ,%end
                      :real-start-pos (or ,start 0))
              ;; declare the variables to be IGNORABLE to prevent the
              ;; compiler from issuing warnings
              (declare
               (ignorable ,match-start ,match-end ,reg-starts ,reg-ends))
              (unless ,match-start
                ;; stop iteration on first failure
                (return ,result-form))
              ;; execute BODY (wrapped in LOCALLY so it can start with
              ;; declarations)
              (locally
                ,@body)
              ;; advance by one position if we had a zero-length match
              (setq ,%start (if (= ,match-start ,match-end)
                              (1+ ,match-end)
                              ,match-end)))))))))

(defmacro do-matches ((match-start match-end regex
                                   target-string
                                   &optional result-form
                                   &key start end)
                      &body body)
  "Iterates over TARGET-STRING and tries to match REGEX as often as
possible evaluating BODY with MATCH-START and MATCH-END bound to the
start/end positions of each match in turn.  After the last match,
returns RESULT-FORM if provided or NIL otherwise.  An implicit block
named NIL surrounds DO-MATCHES; RETURN may be used to terminate the
loop immediately.  If REGEX matches an empty string the scan is
continued one position behind this match.  BODY may start with
declarations."
  ;; this is a simplified form of DO-SCANS - we just provide two dummy
  ;; vars and ignore them
  (with-unique-names (reg-starts reg-ends)
    `(do-scans (,match-start ,match-end
                ,reg-starts ,reg-ends
                ,regex ,target-string
                ,result-form
                :start ,start :end ,end)
      ,@body)))

(defmacro do-matches-as-strings ((match-var regex
                                            target-string
                                            &optional result-form
                                            &key start end sharedp)
                                 &body body)
  "Iterates over TARGET-STRING and tries to match REGEX as often as
possible evaluating BODY with MATCH-VAR bound to the substring of
TARGET-STRING corresponding to each match in turn.  After the last
match, returns RESULT-FORM if provided or NIL otherwise.  An implicit
block named NIL surrounds DO-MATCHES-AS-STRINGS; RETURN may be used to
terminate the loop immediately.  If REGEX matches an empty string the
scan is continued one position behind this match.  If SHAREDP is true,
the substrings may share structure with TARGET-STRING.  BODY may start
with declarations."
  (with-rebinding (target-string)
    (with-unique-names (match-start match-end substr-fn)
      `(let ((,substr-fn (if ,sharedp #'nsubseq #'subseq)))
        ;; simple use DO-MATCHES to extract the substrings
        (do-matches (,match-start ,match-end ,regex ,target-string
                     ,result-form :start ,start :end ,end)
          (let ((,match-var
                  (funcall ,substr-fn
                           ,target-string ,match-start ,match-end)))
            ,@body))))))

(defmacro do-register-groups (var-list (regex target-string
                                              &optional result-form
                                              &key start end sharedp)
                                       &body body)
  "Iterates over TARGET-STRING and tries to match REGEX as often as
possible evaluating BODY with the variables in VAR-LIST bound to the
corresponding register groups for each match in turn, i.e. each
variable is either bound to a string or to NIL.  For each element of
VAR-LIST which is NIL there's no binding to the corresponding register
group. The number of variables in VAR-LIST must not be greater than
the number of register groups.  After the last match, returns
RESULT-FORM if provided or NIL otherwise.  An implicit block named NIL
surrounds DO-REGISTER-GROUPS; RETURN may be used to terminate the loop
immediately. If REGEX matches an empty string the scan is continued
one position behind this match.  If SHAREDP is true, the substrings
may share structure with TARGET-STRING.  BODY may start with
declarations."
  (with-rebinding (target-string)
    (with-unique-names (substr-fn match-start match-end
                                  reg-starts reg-ends start-index)
      `(let ((,substr-fn (if ,sharedp
                          #'nsubseq
                          #'subseq)))
        (do-scans (,match-start ,match-end ,reg-starts ,reg-ends
                                ,regex ,target-string
                                ,result-form :start ,start :end ,end)
          (let ,(loop for (function var) in (normalize-var-list var-list)
                      for counter from 0
                      when var
                        collect `(,var (let ((,start-index
                                               (aref ,reg-starts ,counter)))
                                         (if ,start-index
                                           (funcall ,function
                                                    (funcall ,substr-fn
                                                             ,target-string
                                                             ,start-index
                                                             (aref ,reg-ends ,counter)))
                                           nil))))
            ,@body))))))

(defun all-matches (regex target-string
                          &key (start 0)
                               (end (length target-string)))
  "Returns a list containing the start and end positions of all
matches of REGEX against TARGET-STRING, i.e. if there are N matches
the list contains (* 2 N) elements.  If REGEX matches an empty string
the scan is continued one position behind this match."
  (declare #.*standard-optimize-settings*)
  (let (result-list)
    (do-matches (match-start match-end
                 regex target-string
                 (nreverse result-list)
                 :start start :end end)
      (push match-start result-list)
      (push match-end result-list))))

#-:cormanlisp
(define-compiler-macro all-matches (&whole form &environment env regex &rest rest)
   "Make sure that constant forms are compiled into scanners at
compile time."
   (cond ((constantp regex env)
           `(all-matches (load-time-value (create-scanner ,regex))
                         ,@rest))
         (t form)))

(defun all-matches-as-strings (regex target-string
                                     &key (start 0)
                                          (end (length target-string))
                                          sharedp)
  "Returns a list containing all substrings of TARGET-STRING which
match REGEX. If REGEX matches an empty string the scan is continued
one position behind this match. If SHAREDP is true, the substrings may
share structure with TARGET-STRING."
  (declare #.*standard-optimize-settings*)
  (let (result-list)
    (do-matches-as-strings (match regex target-string (nreverse result-list)
                                  :start start :end end :sharedp sharedp)
      (push match result-list))))

#-:cormanlisp
(define-compiler-macro all-matches-as-strings (&whole form &environment env regex &rest rest)
   "Make sure that constant forms are compiled into scanners at
compile time."
   (cond ((constantp regex env)
           `(all-matches-as-strings
             (load-time-value (create-scanner ,regex))
             ,@rest))
         (t form)))

(defun split (regex target-string
                    &key (start 0)
                         (end (length target-string))
                         limit
                         with-registers-p
                         omit-unmatched-p
                         sharedp)
  "Matches REGEX against TARGET-STRING as often as possible and
returns a list of the substrings between the matches.  If
WITH-REGISTERS-P is true, substrings corresponding to matched
registers are inserted into the list as well.  If OMIT-UNMATCHED-P is
true, unmatched registers will simply be left out, otherwise they will
show up as NIL.  LIMIT limits the number of elements returned -
registers aren't counted.  If LIMIT is NIL \(or 0 which is
equivalent), trailing empty strings are removed from the result list.
If REGEX matches an empty string the scan is continued one position
behind this match.  If SHAREDP is true, the substrings may share
structure with TARGET-STRING."
  (declare #.*standard-optimize-settings*)
  ;; initialize list of positions POS-LIST to extract substrings with
  ;; START so that the start of the next match will mark the end of
  ;; the first substring
  (let ((pos-list (list start))
        (counter 0))
    ;; how would Larry Wall do it?
    (when (eql limit 0)
      (setq limit nil))
    (do-scans (match-start match-end
               reg-starts reg-ends
               regex target-string nil
               :start start :end end)
      (unless (and (= match-start match-end)
                   (= match-start (car pos-list)))
        ;; push start of match on list unless this would be an empty
        ;; string adjacent to the last element pushed onto the list
        (when (and limit
                   (>= (incf counter) limit))
          (return))
        (push match-start pos-list)
        (when with-registers-p
          ;; optionally insert matched registers
          (loop for reg-start across reg-starts
                for reg-end across reg-ends
                if reg-start
                  ;; but only if they've matched
                  do (push reg-start pos-list)
                     (push reg-end pos-list)
                else unless omit-unmatched-p
                  ;; or if we're allowed to insert NIL instead
                  do (push nil pos-list)
                     (push nil pos-list)))
        ;; now end of match
        (push match-end pos-list)))
    ;; end of whole string
    (push end pos-list)
    ;; now collect substrings
    (nreverse
     (loop with substr-fn = (if sharedp #'nsubseq #'subseq)
           with string-seen = nil
           for (this-end this-start) on pos-list by #'cddr
           ;; skip empty strings from end of list
           if (or limit
                  (setq string-seen
                          (or string-seen
                              (and this-start
                                   (> this-end this-start)))))
           collect (if this-start
                     (funcall substr-fn
                              target-string this-start this-end)
                     nil)))))

#-:cormanlisp
(define-compiler-macro split (&whole form &environment env regex target-string &rest rest)
  "Make sure that constant forms are compiled into scanners at compile time."
  (cond ((constantp regex env)
          `(split (load-time-value (create-scanner ,regex))
                  ,target-string ,@rest))
        (t form)))

(defun string-case-modifier (str from to start end)
  (declare #.*standard-optimize-settings*)
  (declare (fixnum from to start end))
  "Checks whether all words in STR between FROM and TO are upcased,
downcased or capitalized and returns a function which applies a
corresponding case modification to strings.  Returns #'IDENTITY
otherwise, especially if words in the target area extend beyond FROM
or TO.  STR is supposed to be bounded by START and END.  It is assumed
that \(<= START FROM TO END)."
  (case
      (if (or (<= to from)
              (and (< start from)
                   (alphanumericp (char str (1- from)))
                   (alphanumericp (char str from)))
              (and (< to end)
                   (alphanumericp (char str to))
                   (alphanumericp (char str (1- to)))))
        ;; if it's a zero-length string or if words extend beyond FROM
        ;; or TO we return NIL, i.e. #'IDENTITY
        nil
        ;; otherwise we loop through STR from FROM to TO
        (loop with last-char-both-case
              with current-result
              for index of-type fixnum from from below to
              for chr = (char str index)
              do (cond ((not #-:cormanlisp (both-case-p chr)
                             #+:cormanlisp (or (upper-case-p chr)
                                               (lower-case-p chr)))
                         ;; this character doesn't have a case so we
                         ;; consider it as a word boundary (note that
                         ;; this differs from how \b works in Perl)
                         (setq last-char-both-case nil))
                       ((upper-case-p chr)
                         ;; an uppercase character
                         (setq current-result
                                 (if last-char-both-case
                                   ;; not the first character in a 
                                   (case current-result
                                     ((:undecided) :upcase)
                                     ((:downcase :capitalize) (return nil))
                                     ((:upcase) current-result))
                                   (case current-result
                                     ((nil) :undecided)
                                     ((:downcase) (return nil))
                                     ((:capitalize :upcase) current-result)))
                               last-char-both-case t))
                       (t
                         ;; a lowercase character
                         (setq current-result
                                 (case current-result
                                   ((nil) :downcase)
                                   ((:undecided) :capitalize)
                                   ((:downcase) current-result)
                                   ((:capitalize) (if last-char-both-case
                                                    current-result
                                                    (return nil)))
                                   ((:upcase) (return nil)))
                               last-char-both-case t)))
              finally (return current-result)))
    ((nil) #'identity)
    ((:undecided :upcase) #'string-upcase)
    ((:downcase) #'string-downcase)
    ((:capitalize) #'string-capitalize)))

;; first create a scanner to identify the special parts of the
;; replacement string (eat your own dog food...)

(defgeneric build-replacement-template (replacement-string)
  (declare #.*standard-optimize-settings*)
  (:documentation "Converts a replacement string for REGEX-REPLACE or
REGEX-REPLACE-ALL into a replacement template which is an
S-expression."))

#-:cormanlisp
(let* ((*use-bmh-matchers* nil)
       (reg-scanner (create-scanner "\\\\(?:\\\\|\\{\\d+\\}|\\d+|&|`|')")))
  (defmethod build-replacement-template ((replacement-string string))
    (declare #.*standard-optimize-settings*)
    (let ((from 0)
          ;; COLLECTOR will hold the (reversed) template
          (collector '()))
      ;; scan through all special parts of the replacement string
      (do-matches (match-start match-end reg-scanner replacement-string)
        (when (< from match-start)
          ;; strings between matches are copied verbatim
          (push (subseq replacement-string from match-start) collector))
        ;; PARSE-START is true if the pattern matched a number which
        ;; refers to a register
        (let* ((parse-start (position-if #'digit-char-p
                                         replacement-string
                                         :start match-start
                                         :end match-end))
               (token (if parse-start
                        (1- (parse-integer replacement-string
                                           :start parse-start
                                           :junk-allowed t))
                        ;; if we didn't match a number we convert the
                        ;; character to a symbol
                        (case (char replacement-string (1+ match-start))
                          ((#\&) :match)
                          ((#\`) :before-match)
                          ((#\') :after-match)
                          ((#\\) :backslash)))))
          (when (and (numberp token) (< token 0))
            ;; make sure we don't accept something like "\\0"
            (signal-invocation-error "Illegal substring ~S in replacement string."
                                     (subseq replacement-string match-start match-end)))
          (push token collector))
        ;; remember where the match ended
        (setq from match-end))
      (when (< from (length replacement-string))
        ;; push the rest of the replacement string onto the list
        (push (subseq replacement-string from) collector))
      (nreverse collector))))

#-:cormanlisp
(defmethod build-replacement-template ((replacement-function function))
  (declare #.*standard-optimize-settings*)
  (list replacement-function))

#-:cormanlisp
(defmethod build-replacement-template ((replacement-function-symbol symbol))
  (declare #.*standard-optimize-settings*)
  (list replacement-function-symbol))
        
#-:cormanlisp
(defmethod build-replacement-template ((replacement-list list))
  (declare #.*standard-optimize-settings*)
  replacement-list)

;;; Corman Lisp's methods can't be closures... :(
#+:cormanlisp
(let* ((*use-bmh-matchers* nil)
       (reg-scanner (create-scanner "\\\\(?:\\\\|\\{\\d+\\}|\\d+|&|`|')")))
  (defun build-replacement-template (replacement)
    (declare #.*standard-optimize-settings*)
    (typecase replacement
      (string
        (let ((from 0)
              ;; COLLECTOR will hold the (reversed) template
              (collector '()))
          ;; scan through all special parts of the replacement string
          (do-matches (match-start match-end reg-scanner replacement)
            (when (< from match-start)
              ;; strings between matches are copied verbatim
              (push (subseq replacement from match-start) collector))
            ;; PARSE-START is true if the pattern matched a number which
            ;; refers to a register
            (let* ((parse-start (position-if #'digit-char-p
                                             replacement
                                             :start match-start
                                             :end match-end))
                   (token (if parse-start
                            (1- (parse-integer replacement
                                               :start parse-start
                                               :junk-allowed t))
                            ;; if we didn't match a number we convert the
                            ;; character to a symbol
                            (case (char replacement (1+ match-start))
                              ((#\&) :match)
                              ((#\`) :before-match)
                              ((#\') :after-match)
                              ((#\\) :backslash)))))
              (when (and (numberp token) (< token 0))
                ;; make sure we don't accept something like "\\0"
                (signal-invocation-error "Illegal substring ~S in replacement string."
                                         (subseq replacement match-start match-end)))
              (push token collector))
            ;; remember where the match ended
            (setq from match-end))
          (when (< from (length replacement))
            ;; push the rest of the replacement string onto the list
            (push (nsubseq replacement from) collector))
          (nreverse collector)))
      (list
        replacement)
      (t
        (list replacement)))))
        
(defun build-replacement (replacement-template
                          target-string
                          start end
                          match-start match-end
                          reg-starts reg-ends
                          simple-calls
                          element-type)
  (declare #.*standard-optimize-settings*)
  "Accepts a replacement template and the current values from the
matching process in REGEX-REPLACE or REGEX-REPLACE-ALL and returns the
corresponding string."
  ;; the upper exclusive bound of the register numbers in the regular
  ;; expression
  (let ((reg-bound (if reg-starts
                     (array-dimension reg-starts 0)
                     0)))
    (with-output-to-string (s nil :element-type element-type)
      (loop for token in replacement-template
            do (typecase token
                 (string
                   ;; transfer string parts verbatim
                   (write-string token s))
                 (integer
                   ;; replace numbers with the corresponding registers
                   (when (>= token reg-bound)
                     ;; but only if the register was referenced in the
                     ;; regular expression
                     (signal-invocation-error "Reference to non-existent register ~A in replacement string."
                                              (1+ token)))
                   (when (svref reg-starts token)
                     ;; and only if it matched, i.e. no match results
                     ;; in an empty string
                     (write-string target-string s
                                   :start (svref reg-starts token)
                                   :end (svref reg-ends token))))
                 (function
                   (write-string 
                    (cond (simple-calls
                           (apply token
                                  (nsubseq target-string match-start match-end)
                                  (map 'list
                                       (lambda (reg-start reg-end)
                                         (and reg-start
                                              (nsubseq target-string reg-start reg-end)))
                                       reg-starts reg-ends)))
                          (t
                           (funcall token
                                    target-string
                                    start end
                                    match-start match-end
                                    reg-starts reg-ends)))
                    s))
                 (symbol
                   (case token
                     ((:backslash)
                       ;; just a backslash
                       (write-char #\\ s))
                     ((:match)
                       ;; the whole match
                       (write-string target-string s
                                     :start match-start
                                     :end match-end))
                     ((:before-match)
                       ;; the part of the target string before the match
                       (write-string target-string s
                                     :start start
                                     :end match-start))
                     ((:after-match)
                       ;; the part of the target string after the match
                       (write-string target-string s
                                     :start match-end
                                     :end end))
                     (otherwise
                      (write-string
                       (cond (simple-calls
                              (apply token
                                     (nsubseq target-string match-start match-end)
                                     (map 'list
                                          (lambda (reg-start reg-end)
                                            (and reg-start
                                                 (nsubseq target-string reg-start reg-end)))
                                          reg-starts reg-ends)))
                             (t
                              (funcall token
                                       target-string
                                       start end
                                       match-start match-end
                                       reg-starts reg-ends)))
                       s)))))))))

(defun replace-aux (target-string replacement pos-list reg-list start end
                                  preserve-case simple-calls element-type)
  "Auxiliary function used by REGEX-REPLACE and REGEX-REPLACE-ALL.
POS-LIST contains a list with the start and end positions of all
matches while REG-LIST contains a list of arrays representing the
corresponding register start and end positions."
  (declare #.*standard-optimize-settings*)
  ;; build the template once before we start the loop
  (let ((replacement-template (build-replacement-template replacement)))
    (with-output-to-string (s nil :element-type element-type)
      ;; loop through all matches and take the start and end of the
      ;; whole string into account
      (loop for (from to) on (append (list start) pos-list (list end))
            ;; alternate between replacement and no replacement
            for replace = nil then (and (not replace) to)
            for reg-starts = (if replace (pop reg-list) nil)
            for reg-ends = (if replace (pop reg-list) nil)
            for curr-replacement = (if replace
                                     ;; build the replacement string
                                     (build-replacement replacement-template
                                                        target-string
                                                        start end
                                                        from to
                                                        reg-starts reg-ends
                                                        simple-calls
                                                        element-type)
                                     nil)
            while to
            if replace
              do (write-string (if preserve-case
                                 ;; modify the case of the replacement
                                 ;; string if necessary
                                 (funcall (string-case-modifier target-string
                                                                from to
                                                                start end)
                                          curr-replacement)
                                 curr-replacement)
                               s)
            else
              ;; no replacement
              do (write-string target-string s :start from :end to)))))

(defun regex-replace (regex target-string replacement &key
                            (start 0)
                            (end (length target-string))
                            preserve-case
                            simple-calls
                            (element-type #+:lispworks 'lw:simple-char #-:lispworks 'character))
  "Try to match TARGET-STRING between START and END against REGEX and
replace the first match with REPLACEMENT.  Two values are returned;
the modified string, and T if REGEX matched or NIL otherwise.

  REPLACEMENT can be a string which may contain the special substrings
\"\\&\" for the whole match, \"\\`\" for the part of TARGET-STRING
before the match, \"\\'\" for the part of TARGET-STRING after the
match, \"\\N\" or \"\\{N}\" for the Nth register where N is a positive
integer.

  REPLACEMENT can also be a function designator in which case the
match will be replaced with the result of calling the function
designated by REPLACEMENT with the arguments TARGET-STRING, START,
END, MATCH-START, MATCH-END, REG-STARTS, and REG-ENDS. (REG-STARTS and
REG-ENDS are arrays holding the start and end positions of matched
registers or NIL - the meaning of the other arguments should be
obvious.)

  Finally, REPLACEMENT can be a list where each element is a string,
one of the symbols :MATCH, :BEFORE-MATCH, or :AFTER-MATCH -
corresponding to \"\\&\", \"\\`\", and \"\\'\" above -, an integer N -
representing register (1+ N) -, or a function designator.

  If PRESERVE-CASE is true, the replacement will try to preserve the
case (all upper case, all lower case, or capitalized) of the
match. The result will always be a fresh string, even if REGEX doesn't
match.

  ELEMENT-TYPE is the element type of the resulting string."
  (declare #.*standard-optimize-settings*)
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (scan regex target-string :start start :end end)
    (if match-start
      (values (replace-aux target-string replacement
                           (list match-start match-end)
                           (list reg-starts reg-ends)
                           start end preserve-case
                           simple-calls element-type)
              t)
      (values (subseq target-string start end)
              nil))))

#-:cormanlisp
(define-compiler-macro regex-replace
    (&whole form &environment env regex target-string replacement &rest rest)
  "Make sure that constant forms are compiled into scanners at compile time."
  (cond ((constantp regex env)
          `(regex-replace (load-time-value (create-scanner ,regex))
                          ,target-string ,replacement ,@rest))
        (t form)))

(defun regex-replace-all (regex target-string replacement &key
                                (start 0)
                                (end (length target-string))
                                preserve-case
                                simple-calls
                                (element-type #+:lispworks 'lw:simple-char #-:lispworks 'character))
  "Try to match TARGET-STRING between START and END against REGEX and
replace all matches with REPLACEMENT.  Two values are returned; the
modified string, and T if REGEX matched or NIL otherwise.

  REPLACEMENT can be a string which may contain the special substrings
\"\\&\" for the whole match, \"\\`\" for the part of TARGET-STRING
before the match, \"\\'\" for the part of TARGET-STRING after the
match, \"\\N\" or \"\\{N}\" for the Nth register where N is a positive
integer.

  REPLACEMENT can also be a function designator in which case the
match will be replaced with the result of calling the function
designated by REPLACEMENT with the arguments TARGET-STRING, START,
END, MATCH-START, MATCH-END, REG-STARTS, and REG-ENDS. (REG-STARTS and
REG-ENDS are arrays holding the start and end positions of matched
registers or NIL - the meaning of the other arguments should be
obvious.)

  Finally, REPLACEMENT can be a list where each element is a string,
one of the symbols :MATCH, :BEFORE-MATCH, or :AFTER-MATCH -
corresponding to \"\\&\", \"\\`\", and \"\\'\" above -, an integer N -
representing register (1+ N) -, or a function designator.

  If PRESERVE-CASE is true, the replacement will try to preserve the
case (all upper case, all lower case, or capitalized) of the
match. The result will always be a fresh string, even if REGEX doesn't
match.

  ELEMENT-TYPE is the element type of the resulting string."
  (declare #.*standard-optimize-settings*)
  (let ((pos-list '())
        (reg-list '()))
    (do-scans (match-start match-end reg-starts reg-ends regex target-string
                           nil
                           :start start :end end)
      (push match-start pos-list)
      (push match-end pos-list)
      (push reg-starts reg-list)
      (push reg-ends reg-list))
    (if pos-list
      (values (replace-aux target-string replacement
                           (nreverse pos-list)
                           (nreverse reg-list)
                           start end preserve-case
                           simple-calls element-type)
              t)
      (values (subseq target-string start end)
              nil))))

#-:cormanlisp
(define-compiler-macro regex-replace-all
    (&whole form &environment env regex target-string replacement &rest rest)
  "Make sure that constant forms are compiled into scanners at compile time."
  (cond ((constantp regex env)
          `(regex-replace-all (load-time-value (create-scanner ,regex))
                              ,target-string ,replacement ,@rest))
        (t form)))

#-:cormanlisp
(defmacro regex-apropos-aux ((regex packages case-insensitive &optional return-form)
                             &body body)
  "Auxiliary macro used by REGEX-APROPOS and REGEX-APROPOS-LIST. Loops
through PACKAGES and executes BODY with SYMBOL bound to each symbol
which matches REGEX. Optionally evaluates and returns RETURN-FORM at
the end. If CASE-INSENSITIVE is true and REGEX isn't already a
scanner, a case-insensitive scanner is used."
  (with-rebinding (regex)
    (with-unique-names (scanner %packages next morep hash)
      `(let* ((,scanner (create-scanner ,regex
                                        :case-insensitive-mode
                                        (and ,case-insensitive
                                             (not (functionp ,regex)))))
              (,%packages (or ,packages
                              (list-all-packages)))
              (,hash (make-hash-table :test #'eq)))
         (with-package-iterator (,next ,%packages :external :internal :inherited)
           (loop
             (multiple-value-bind (,morep symbol)
                 (,next)
               (unless ,morep
                 (return ,return-form))
               (unless (gethash symbol ,hash)
                 (when (scan ,scanner (symbol-name symbol))
                   (setf (gethash symbol ,hash) t)
                   ,@body)))))))))

;;; The following two functions were provided by Karsten Poeck

#+:cormanlisp
(defmacro do-with-all-symbols ((variable package-or-packagelist) &body body)
  "Executes BODY with VARIABLE bound to each symbol in
PACKAGE-OR-PACKAGELIST \(a designator for a list of packages) in
turn."
  (with-unique-names (pack-var)
    `(if (listp ,package-or-packagelist)
      (dolist (,pack-var ,package-or-packagelist)
        (do-symbols (,variable ,pack-var)
          ,@body))
      (do-symbols (,variable ,package-or-packagelist)
        ,@body))))

#+:cormanlisp
(defmacro regex-apropos-aux ((regex packages case-insensitive &optional return-form)
                             &body body)
  "Auxiliary macro used by REGEX-APROPOS and REGEX-APROPOS-LIST.
Loops through PACKAGES and executes BODY with SYMBOL bound to each
symbol which matches REGEX.  Optionally evaluates and returns
RETURN-FORM at the end.  If CASE-INSENSITIVE is true and REGEX isn't
already a scanner, a case-insensitive scanner is used."
  (with-rebinding (regex)
    (with-unique-names (scanner %packages hash)
      `(let* ((,scanner (create-scanner ,regex
                         :case-insensitive-mode
                         (and ,case-insensitive
                              (not (functionp ,regex)))))
              (,%packages (or ,packages
                             (list-all-packages)))
              (,hash (make-hash-table :test #'eq)))
        (do-with-all-symbols (symbol ,%packages)
          (unless (gethash symbol ,hash)
            (when (scan ,scanner (symbol-name symbol))
              (setf (gethash symbol ,hash) t)
              ,@body)))
        ,return-form))))

(defun regex-apropos-list (regex &optional packages &key (case-insensitive t))
  (declare #.*standard-optimize-settings*)
  "Similar to the standard function APROPOS-LIST but returns a list of
all symbols which match the regular expression REGEX.  If
CASE-INSENSITIVE is true and REGEX isn't already a scanner, a
case-insensitive scanner is used."
  (let ((collector '()))
    (regex-apropos-aux (regex packages case-insensitive collector)
      (push symbol collector))))

(defun print-symbol-info (symbol)
  "Auxiliary function used by REGEX-APROPOS. Tries to print some
meaningful information about a symbol."
  (declare #.*standard-optimize-settings*)
  (handler-case
    (let ((output-list '()))
      (cond ((special-operator-p symbol)
              (push "[special operator]" output-list))
            ((macro-function symbol)
              (push "[macro]" output-list))
            ((fboundp symbol)
              (let* ((function (symbol-function symbol))
                     (compiledp (compiled-function-p function)))
                (multiple-value-bind (lambda-expr closurep)
                    (function-lambda-expression function)
                  (push
                    (format nil
                            "[~:[~;compiled ~]~:[function~;closure~]]~:[~; ~A~]"
                            compiledp closurep lambda-expr (cadr lambda-expr))
                    output-list)))))
      (let ((class (find-class symbol nil)))
        (when class
          (push (format nil "[class] ~S" class) output-list)))
      (cond ((keywordp symbol)
              (push "[keyword]" output-list))
            ((constantp symbol)
              (push (format nil "[constant]~:[~; value: ~S~]"
                            (boundp symbol) (symbol-value symbol)) output-list))
            ((boundp symbol)
              (push #+(or :lispworks :clisp) "[variable]"
                    #-(or :lispworks :clisp) (format nil "[variable] value: ~S"
                                                   (symbol-value symbol))
                    output-list)))
      #-(or :cormanlisp :clisp)
      (format t "~&~S ~<~;~^~A~@{~:@_~A~}~;~:>" symbol output-list)
      #+(or :cormanlisp :clisp)
      (loop for line in output-list
            do (format t "~&~S ~A" symbol line)))
    (condition ()
      ;; this seems to be necessary due to some errors I encountered
      ;; with LispWorks
      (format t "~&~S [an error occurred while trying to print more info]" symbol))))

(defun regex-apropos (regex &optional packages &key (case-insensitive t))
  "Similar to the standard function APROPOS but returns a list of all
symbols which match the regular expression REGEX.  If CASE-INSENSITIVE
is true and REGEX isn't already a scanner, a case-insensitive scanner
is used."
  (declare #.*standard-optimize-settings*)
  (regex-apropos-aux (regex packages case-insensitive)
    (print-symbol-info symbol))
  (values))

(let* ((*use-bmh-matchers* nil)
       (non-word-char-scanner (create-scanner "[^a-zA-Z_0-9]")))
  (defun quote-meta-chars (string &key (start 0) (end (length string)))
    "Quote, i.e. prefix with #\\\\, all non-word characters in STRING."
    (regex-replace-all non-word-char-scanner string "\\\\\\&"
                       :start start :end end)))

(let* ((*use-bmh-matchers* nil)
       (*allow-quoting* nil)
       (quote-char-scanner (create-scanner "\\\\Q"))
       (section-scanner (create-scanner "\\\\Q((?:[^\\\\]|\\\\(?!Q))*?)(?:\\\\E|$)")))
  (defun quote-sections (string)
    "Replace sections inside of STRING which are enclosed by \\Q and
\\E with the quoted equivalent of these sections \(see
QUOTE-META-CHARS). Repeat this as long as there are such
sections. These sections may nest."
    (flet ((quote-substring (target-string start end match-start
                                           match-end reg-starts reg-ends)
             (declare (ignore start end match-start match-end))
             (quote-meta-chars target-string
                               :start (svref reg-starts 0)
                               :end (svref reg-ends 0))))
      (loop for result = string then (regex-replace-all section-scanner
                                                        result
                                                        #'quote-substring)
            while (scan quote-char-scanner result)
            finally (return result)))))

(let* ((*use-bmh-matchers* nil)
       (comment-scanner (create-scanner "(?s)\\(\\?#.*?\\)"))
       (extended-comment-scanner (create-scanner "(?m:#.*?$)|(?s:\\(\\?#.*?\\))"))
       (quote-token-scanner (create-scanner "\\\\[QE]"))
       (quote-token-replace-scanner (create-scanner "\\\\([QE])")))
  (defun clean-comments (string &optional extended-mode)
    "Clean \(?#...) comments within STRING for quoting, i.e. convert
\\Q to Q and \\E to E.  If EXTENDED-MODE is true, also clean
end-of-line comments, i.e. those starting with #\\# and ending with
#\\Newline."
    (flet ((remove-tokens (target-string start end match-start
                                         match-end reg-starts reg-ends)
             (declare (ignore start end reg-starts reg-ends))
             (loop for result = (nsubseq target-string match-start match-end)
                   then (regex-replace-all quote-token-replace-scanner result "\\1")
                   ;; we must probably repeat this because the comment
                   ;; can contain substrings like \\Q
                   while (scan quote-token-scanner result)
                   finally (return result))))
      (regex-replace-all (if extended-mode
                           extended-comment-scanner
                           comment-scanner)
                         string
                         #'remove-tokens))))

(defun parse-tree-synonym (symbol)
  "Returns the parse tree the SYMBOL symbol is a synonym for.  Returns
NIL is SYMBOL wasn't yet defined to be a synonym."
  (get symbol 'parse-tree-synonym))

(defun (setf parse-tree-synonym) (new-parse-tree symbol)
  "Defines SYMBOL to be a synonm for the parse tree NEW-PARSE-TREE."
  (setf (get symbol 'parse-tree-synonym) new-parse-tree))

(defmacro define-parse-tree-synonym (name parse-tree)
  "Defines the symbol NAME to be a synonym for the parse tree
PARSE-TREE.  Both arguments are quoted."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (parse-tree-synonym ',name) ',parse-tree)))
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
