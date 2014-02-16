(in-package #:mex)

;;; Character category tables

(eval-when (:compile-toplevel :load-toplevel :execute)

  (ambi-ps ()

    ;; A category is represented as a number which encodes 1) the basic kind
    ;; of character; 2) whether it is “active”, that is, can be used as a
    ;; command on its own; 3) whether it is “constituent”, that is, can be
    ;; used as part of a multi-character name of a command after an escape
    ;; character.

    (defenum ccat-base ()
      *ccat-invalid*
      *ccat-whitespace*
      *ccat-newline*
      *ccat-escape*
      *ccat-param*
      *ccat-lbrace*
      *ccat-rbrace*
      *ccat-letter*
      *ccat-number*
      *ccat-other*)

    (defvar *ccat-active*
      (ash 1 (ceiling (log (1+ *ccat-other*) 2))))

    (defvar *ccat-constituent*
      (ash *ccat-active* 1))

    (defvar *ccat-letter-constituent*
      (logior *ccat-letter* *ccat-constituent*))

    (defun ccat-base (cat)
      "Return the base part of category CAT."
      (logandc2 cat (logior *ccat-active* *ccat-constituent*)))

    (defun ccat-constituent-p (cat &optional requisite-base)
      "Return true iff CAT is a constituent category whose base part is the
       same as REQUISITE-BASE (if supplied and positive) or different from
       - REQUISITE-BASE (if supplied and negative)."
      (and (> (logand cat *ccat-constituent*) 0)
           (cond ((null requisite-base))
                 ((minusp requisite-base)
                  (not (eq (ccat-base cat) (- requisite-base))))
                 (t (eq (ccat-base cat) requisite-base)))))

    (defun ccat-active-p (cat)
      "Return true iff CAT is an active category."
      (> (logand cat *ccat-active*) 0))

    (defstruct (cat-table (:conc-name))
      "A table assigning CATEGORY to all characters with codes in the range
       between MIN-CHAR and MAX-CHAR (inclusive), except in subranges
       specified by EXCEPTIONS (which should be a vector of category tables,
       sorted by MIN-CHAR)."
      min-char max-char category exceptions)

    (defun cat-point (code cat)
      "Return a category table which assigns category CAT to just one code
       point CODE."
      (make-cat-table :min-char code :max-char code :category cat))

    (defun call-with-cat-table-range-divided (tables low high fn)
      "Take a sequence of category TABLES (sorted by MIN-CHAR), two
       character codes LOW and HIGH, and a function FN.  Call FN, passing it
       eight arguments representing a five-fold partition: 1‒2) the indices
       in TABLES of the first and last (exclusive) members which are
       strictly inferior to LOW; 3) the index of a member which surrounds
       LOW; 4‒5) the indices of the first and last (exclusive) members which
       are above LOW and strictly below HIGH; 6) the index of a member which
       surrounds HIGH; 7) the index of the first member which is strictly
       above HIGH; and 8) the length of TABLES.  (If any of the five parts
       are empty, the corresponding arguments to FN are NULL.  The arguments
       3 and 6 may be the same number, if LOW and HIGH are both contained
       within one of the TABLES.)  The value returned by FN is also the
       value of CALL-WITH-CAT-TABLE-RANGE-DIVIDED."
      (loop with i := 0 while (< i (length tables))
            with left-start := nil with left-end := nil
            with left-edge := nil with right-edge := nil
            with body-start := nil with body-end := nil 
            with right-start := nil with right-end := (length tables)
            with state := :left
            do (let* ((tbl (aref tables i))
                      (minc (min-char tbl))
                      (maxc (max-char tbl)))
                 (case state
                   (:left
                    (if (< maxc low)
                        (progn
                          (unless left-start (setf left-start i))
                          (incf i))
                        (progn
                          (when left-start (setf left-end i))
                          (when (<= minc low)
                            (setf left-edge i)
                            (unless (> maxc high)
                              (incf i)))
                          (setf state :body))))
                   (:body
                    (if (<= maxc high)
                        (progn
                          (unless body-start (setf body-start i))
                          (incf i))
                        (progn
                          (when body-start (setf body-end i))
                          (when (<= minc high)
                            (setf right-edge i)
                            (incf i))
                          (setf state :right))))
                   (:right
                    (setf right-start i)
                    (setf i (length tables)))))
            finally (return
                      (funcall fn
                        left-start left-end left-edge body-start
                        body-end right-edge right-start right-end))))

    (defun cat-table-divide (ct pivot cont)
      "Call CONT on two category tables: 1) representing the part of the
       range of characters in the table CT strictly below character code
       PIVOT; and 2) representing the part of the range above and including
       PIVOT."
      (let ((ct-left (copy-structure ct))
            (ct-right (copy-structure ct))
            (exceptions (or (exceptions ct) #())))
        (setf (max-char ct-left) (1- pivot)
              (min-char ct-right) pivot)
        ;; The exceptions are also split between the two tables.
        (call-with-cat-table-range-divided exceptions pivot (1- pivot)
         (lambda (left-start left-end left-edge body-start
                  body-end right-edge right-start right-end)
           (declare (ignore body-start body-end right-edge))
           (flet ((take (start end)
                    (and start
                         (sliced exceptions start (or end right-end)))))
             (let ((left (take left-start left-end))
                   (right (take right-start right-end)))
               ;; An exception surrounding PIVOT is further divided, and
               ;; each part appended to the exceptions on its own side.
               (if left-edge
                   (cat-table-divide (aref exceptions left-edge) pivot
                     (lambda (el er)
                       (setf (exceptions ct-left)
                               (vector-add left (vector el))
                             (exceptions ct-right)
                               (vector-add (vector er) right))))
                   (setf (exceptions ct-left) left
                         (exceptions ct-right) right))))))
        (funcall cont ct-left ct-right)))

    (declaim (ftype function cat-table))

    (defun cat-table-concat (ct1 ct2 &optional (seam-cat *ccat-invalid*))
      "Return a category table representing the range of characters which
       includes the ranges of the tables CT1 and CT2.  If CT1 and CT2 are
       not strictly adjacent, all characters in between them are assigned
       the category SEAM-CAT."
      (let* ((minc1 (min-char ct1)) (minc2 (min-char ct2))
             (maxc1 (max-char ct1)) (maxc2 (max-char ct2))
             ;; Just in case the tables overlap...
             (disjoint (or (<= maxc1 minc2) (<= maxc2 minc1))))
        ;; If CT1 is above CT2, swap.
        (unless (<= minc1 minc2)
          (let ((minc-tmp minc1)) (setf minc1 minc2 minc2 minc-tmp))
          (let ((maxc-tmp maxc1)) (setf maxc1 maxc2 maxc2 maxc-tmp))
          (let ((ct-tmp ct1)) (setf ct1 ct2 ct2 ct-tmp)))
        (let ((cat1 (category ct1)) (cat2 (category ct2))
              (exc1 (exceptions ct1)) (exc2 (exceptions ct2))
              (minc (min minc1 minc2)) (maxc (max maxc1 maxc2)))
          (if (and (eq cat1 cat2) disjoint)
              (cat-table minc maxc cat1
                         (if (< maxc1 (1- minc2))
                             (let ((seam (cat-table (1+ maxc1) (1- minc2)
                                                    seam-cat)))
                               (vector-add exc1 (vector seam) exc2))
                             (vector-add exc1 exc2)))
              (cat-table minc maxc seam-cat (vector ct1 ct2))))))

  )

  (defmethod print-object ((ct cat-table) stream)
    (if *print-readably* (call-next-method)
        (print-unreadable-object (ct stream :type t)
          (labels ((print-level (ct)
                     (let ((minc (min-char ct))
                           (maxc (max-char ct))
                           (cat (category ct)))
                       (if (= minc maxc)
                           (format stream "~A = ~A" minc cat)
                           (format stream "~A..~A = ~A" minc maxc cat)))
                     (when (exceptions ct)
                       (format stream " /")
                       (loop for exc :across (exceptions ct)
                             do (format stream " [")
                                (print-level exc)
                                (format stream "]")))))
            (print-level ct)))))

  (defun cat-table (minc maxc cat &optional exceptions)
    "Create a category table with the given bounds MINC, MAXC, category
     CAT, and EXCEPTIONS."
    (make-cat-table
     :min-char minc :max-char maxc :category cat
     :exceptions (if (not exceptions) nil
                     (ensure-vector exceptions))))

  ;; The default category table shall be built from Unicode data.

  (defun unicode-cat-to-mex-cat (cat)
    "Take a Unicode category descriptor (such as \"Mc\" for Spacing
     Combining Mark), and return a Mex category number."
    (let* ((cat* (if (and (stringp cat) (= (length cat) 2)) cat "Cn"))
           (cat0 (aref cat* 0))
           (cat1 (aref cat* 1)))
      (case cat0
        ((#\L) *ccat-letter-constituent*)
        ((#\M) *ccat-other*)
        ((#\P #\S) *ccat-other*)
        ((#\Z) (if (char= cat1 #\s) *ccat-whitespace* *ccat-newline*))
        ((#\N) *ccat-number*)
        ((#\C) (if (or (char= cat1 #\c) (char= cat1 #\n)) *ccat-invalid*
                   *ccat-other*))
        (t *ccat-invalid*))))

  (defun exceptional-mex-cat (c)
    "For some characters that have special meanings in Mex, return the
     respective category numbers."
    (case c
      ((#.(code-char 9))                      ; Tab
       *ccat-whitespace*)
      ((#.(code-char 13) #.(code-char 10))    ; CR, LF (cf. Tokens)
       (logior *ccat-newline* *ccat-constituent*))
      ((#.(code-char 160) #.(code-char 8239)) ; No-break spaces
       (logior *ccat-whitespace* *ccat-constituent*))
      (#\\ *ccat-escape*)
      (#\{ *ccat-lbrace*)
      (#\} *ccat-rbrace*)
      (#\# *ccat-param*)))

  (defun normalize-cat-table-mapping (mapping main-cat)
    "Sort and optimize the sequence of category tables MAPPING so as to put
     them as exceptions into a table with category MAIN-CAT."
    (setf mapping (sort mapping #'< :key #'min-char))
    ;; Insert explicit invalid subblocks, if necessary.
    (unless (eq main-cat *ccat-invalid*)
      (loop for c-spec :in mapping
            with prev-c-spec := nil
            with minc and prev-maxc
            when prev-c-spec
              do (setf prev-maxc (max-char prev-c-spec)
                       minc (min-char c-spec))
              and when (/= minc (1+ prev-maxc))
                collect (cat-table (1+ prev-maxc) (1- minc) *ccat-invalid*)
                  :into mapping+
            collect (setf prev-c-spec c-spec) :into mapping+
            finally (setf mapping mapping+)))
    ;; Compact the mapping by 1) eliminating “exceptions” which would have
    ;; the same category as the superordinate table; and 2) merging adjacent
    ;; exceptions that have the same category into subblocks.
    (loop for c-spec :in mapping
          with prev-c-spec := (cat-table -1 -1 -1)
          for cat := (category c-spec)
          unless (eq cat main-cat)
            if (and (eq cat (category prev-c-spec))
                    (= (min-char c-spec) (1+ (max-char prev-c-spec))))
              do (setf (max-char prev-c-spec) (max-char c-spec))
            else collect c-spec
              and do (setf prev-c-spec c-spec)))

  (defun cat-table-unicode-block-spec (block-name)
    "Take a Unicode block name and return a category table specifying the
     categories for all characters in that block."
    (format *error-output*
            "~&; preparing MEX category data in Unicode block ~A...~%"
            block-name)
    (finish-output *error-output*)
    (let ((block-chars (list-all-characters
                        (format nil "Block:~a" block-name))))
      (loop with cats := '()
            for c :in block-chars
            for code := (char-code c)
            for cat := (or (exceptional-mex-cat c)
                           (unicode-cat-to-mex-cat (general-category c)))
            minimize code :into minc
            maximize code :into maxc
            do (incf (getf cats cat 0))
            ;; Initially, all characters are considered exceptional.  Since
            ;; CL-UNICODE:LIST-ALL-CHARACTERS does not guarantee that the
            ;; list shall be sorted by character code, we don't assume that
            ;; either.  The sorting and optimization is done in the function
            ;; NORMALIZE-CAT-TABLE-MAPPING.
            collect (cat-point code cat) :into mapping
            finally
              (let* ((main-cat
                      ;; The category of the majority of characters is the
                      ;; category of the block.
                      (loop for (cat n) :on cats :by #'cddr
                            with max-cat := -1 and max-n := -1
                            if (>= n max-n) do (setf max-cat cat max-n n)
                            finally (return max-cat)))
                     (mapping
                      (normalize-cat-table-mapping mapping main-cat)))
                (return (cat-table minc maxc main-cat mapping))))))

  (defun cat-table-from-unicode-blocks (&rest blocks)
    "Take some Unicode block names and make a category table for the whole
     Unicode code point range, using Unicode data for the included
     characters to initialize the categories in the table."
    (let ((block-tables (sort (mapcar #'cat-table-unicode-block-spec
                                      (or blocks (code-blocks)))
                              #'< :key #'min-char)))
      (format *error-output* "~&; optimizing MEX category data...~%")
      (cat-table
       0 (1- +code-point-limit+) *ccat-invalid*
       (reduce
        (lambda (block-table tables)
          (let ((prev-table (car tables)))
            ;; Merge adjacent blocks that have the same category.
            (if (and prev-table
                     (eq (category block-table) (category prev-table))
                     (= (min-char block-table) (1+ (max-char prev-table))))
                (let ((table+ (cat-table-concat block-table prev-table)))
                  (cons table+ (cdr tables)))
                (cons block-table tables))))
        block-tables
        :initial-value '()
        :from-end t))))

)


(defvar *default-category-table*
  #.(cat-table-from-unicode-blocks "Basic Latin" "Cyrillic"
                                   "Latin-1 Supplement")
  "Category table with definitions from Unicode.")

(defvar *plain-category-table*
  #.(cat-table 0 (1- +code-point-limit+) *ccat-other*)
  "Category table where every character is categorized as non-active
   non-costituent other.")

(ambi-ps ()

  (defvar *category-table* (struct *default-category-table*)
    "The standard initial category table.")

  (defun char-cat (char &optional (table *category-table*)
                                  (cat nil assign))
    "If called with one or two arguments, return the category of the given
     character or code CHAR in the given TABLE.  If called with three
     arguments, return a new table resulting from assigning CHAR the
     category CAT in TABLE."
    (let ((code (if (numberp char) char (char-code char)))
          (exceptions (exceptions table)))
      (call-with-cat-table-range-divided exceptions code code
        (lambda (left-start left-end left-edge body-start
                 body-end right-edge right-start right-end)
          (declare (ignore left-start left-end body-end right-edge))
          #|
          (when (member 'char-cat (trace))
            (format *trace-output* "~&**** LEFT-EDGE=~S~@[=~S~] BODY-START=~S~@[=~S~]~%"
                    left-edge (and left-edge (aref exceptions left-edge))
                    body-start (and body-start (aref exceptions body-start)))
            (format *trace-output* "~&**** RIGHT-START=~S~@[=~S~] RIGHT-END=~S~%"
                    right-start (and right-start (aref exceptions right-start))
                    right-end))
           |#
          (let ((matching-exception (or left-edge body-start)))
            (if assign
                (if matching-exception
                    (let* ((exc (aref exceptions matching-exception))
                           (exc+ (char-cat code exc cat)))
                      (if (eq exc exc+) table
                          (let ((table+ (copy-structure table)))
                            (setf (exceptions table+)
                                    (spliced exceptions matching-exception
                                             1 exc+))
                            table+)))
                    (if (eq cat (category table)) table
                        (let ((table+ (copy-structure table)))
                          (setf (exceptions table+)
                                  (spliced exceptions
                                           (or right-start right-end)
                                           0 (cat-point code cat)))
                          table+)))
                (if matching-exception
                    (char-cat code (aref exceptions matching-exception))
                    (category table))))))))

)

;;; Local Variables: ***
;;; mode:lisp ***
;;; local-font-lock-keywords:(("(\\(def\\(?:\\(ps\\(?:macro\\|fun\\)\\|\\(?:un\\|macro\\)\\+ps\\)\\|\\(enum\\)\\)\\)[ \t\n]+\\([^()]*?\\)[ \t\n]" (1 font-lock-keyword-face) (4 (cond ((match-beginning 3) font-lock-variable-name-face)))) ("(\\(ambi-ps\\)\\_>" (1 font-lock-preprocessor-face))) ***
;;; End: ***
