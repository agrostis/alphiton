(in-package #:mex)

;;; Lisp / JS compatibility

;; Major parts of this program are written in a compromise idiom, an
;; intersection of Lisp and ParenScript.  The following definitions extend
;; either of the two languages where this is necessary to upkeep the
;; “ambilingual” style.

(defparameter *js-target* nil
  "List of source file pathnames and strings with JS code generated by
   AMBI-PS.")

(defun push-js (js target)
  (let ((lim 200))
    (pushnew
      (cons (and *load-pathname* (pathname-name *load-pathname*)) js)
      target
      :test #'string=
      :key (lambda (p+s)
             (let ((s (cdr p+s)))
               (if (<= (length s) lim) s (subseq s 0 lim)))))))

(defun print-as-js-comment (docstring out)
  (terpri out)
  (write-line "/** " out)
  (dolist (line (cl-ppcre:split #\Newline docstring))
    (princ " ** " out)
    (princ line out)
    (terpri out))
  (write-line " **/" out)
  (terpri out))

(defmacro ambi-ps ((&optional (target nil targetp)) &body body)
  "If TARGET is NIL, evaluate BODY as Lisp code.  If TARGET is a stream or
   T, compile BODY as ParenScript code and write the resulting JavaScript
   to the stream or to standard output.  If no TARGET is given, do both,
   appending the JavaScript output to *JS-TARGET*."
  (if targetp
      (if target
          (let ((ps-body-to-stream (gensym "PS-BODY-TO-STREAM-"))
                (docstring (when (stringp (car body)) (pop body))))
            `(flet ((,ps-body-to-stream (out)
                      ,(when docstring
                         `(print-as-js-comment ,docstring out))
                      (ps-to-stream out ,@body)))
               ,(case target
                  ((t) `(,ps-body-to-stream *standard-output*))
                  ((*js-target*) `(let ((js (with-output-to-string (s)
                                              (,ps-body-to-stream s)
                                              (terpri s))))
                                    (setf *js-target*
                                            (push-js js *js-target*))
                                    js))
                  (t `(,ps-body-to-stream ,target)))))
          `(macrolet ((lisp (expr) expr)) ,@body))
      `(progn
         (ambi-ps (nil) ,@body)
         (ambi-ps (*js-target*) ,@body))))

(defmacro defpsfun (name (&rest args) &body body)
  "Appends a JavaScript function definition to *JS-TARGET*."
  `(ambi-ps (*js-target*)
     (defun ,name (,@args) ,@body)))

(defmacro defpsvar (name &optional init doc)
  "Appends a JavaScript variable definition to *JS-TARGET*."
  `(ambi-ps (*js-target*)
     (defvar ,name ,@(when init `(,init)) ,@(when doc `(,doc)))))

(defmacro defun+ps (name (&rest args) &body body)
  "Appends a JavaScript function definition to *JS-TARGET* and does a Lisp
   DEFUN."
  `(ambi-ps ()
     (defun ,name (,@args) ,@body)))

(defun js-exports (package)
  (let ((exports nil) (new (ps-gensym)))
    (do-external-symbols (s package)
      (let ((name (symbol-name s)))
        (unless (char= (aref name (1- (length name))) #\%)
          (cond
            ((boundp s)
             (setf (getf exports `(get ,s)) s)
             (setf (getf exports `(set ,s ,new)) `(setf ,s ,new)))
            ((fboundp s)
             (unless (macro-function s)
               (setf (getf exports s) s)))))))
    (ps:ps* `(ps-js:return (create ,@exports)))))

(defun js-target-concat ()
  (with-output-to-string (out)
    (loop for ((path . code) . more) :on (reverse *js-target*)
          for path* = (or path (car (find-if #'car more)))
          with prev-path := nil
          if (not (equal path* prev-path))
            do (setq prev-path path*)
               (format out "~%/**************** ~A ****************/~%~%"
                       path*)
          do (format out "~A~%" code))))

(defun write-js% (js-file)
  (when *js-target*
    (format *error-output*
            "~&~%; writing generated JavaScript to ~S...~%"
            (enough-namestring js-file))
    (let ((target-code (js-target-concat))
          (exports (js-exports '#:mex)))
      (if (probe-file js-file)
          (linewise-template:process-template
              ((:file js-file :update t
                :circumfix '("/(\\*{8,}) " " \\1/") :escape nil)
                   :copy
                   ((:block code)
                        :replace :with target-code
                        :preserve-directives t)
                   ((:block export)
                        :replace :with exports
                        :preserve-directives t)))
          (with-open-file (out js-file :direction :output)
            (princ generated))))))

(defmacro+ps lisp* (form)
  "Same as (LISP FORM), but used for its side effects rather than its
   value."
  `(lisp (progn ,form '(progn))))

(defpsmacro declaim (&rest declarations)
  (declare (ignore declarations))
  "Ignore DECLAIMs in JavaScript.")

(defun form-with-var-value (expr form-fn)
  "FORM-FN should be a function which takes an expression and produces
   some code including that expression.  If EXPR is a compound expression,
   return a local binding form wrapping the code produced by a call to
   FORM-FN with the local variable as argument.  If EXPR is a simple
   expression, call FORM-FN directly with EXPR as argument, and return the
   result."
  (if (or (atom expr) (and (listp expr) (eq (car expr) '@)))
      (funcall form-fn expr)
      (let ((var (ps-gensym)))
        `(let ((,var ,expr)) ,(funcall form-fn var)))))

(defmacro with-var-value ((var) form)
  "Wrapper for FORM-WITH-VAR-VALUE."
  `(form-with-var-value ,var (lambda (,var) ,form)))

(defvar *standard-symbol-to-js*
  (symbol-function 'symbol-to-js-string))

(defun symbol-to-js-string (symbol &optional (mangle t))
  (let ((name (symbol-name symbol))
        (pkg (symbol-package symbol)))
    (multiple-value-bind (bare-name modifier)
        (multiple-value-bind (match regs)
            (ppcre:scan-to-strings
              "^([^*].*)(?:(-)|([*+%])([0-9]*))$|^\\*.*\\*$"
              name)
          (if match
              (if (aref regs 0)
                  (values (concatenate 'string
                            (aref regs 0) (or (aref regs 3) ""))
                          (case (aref (or (aref regs 1) (aref regs 2)) 0)
                            (#\* #.(code-char #x033D))
                            (#\+ #.(code-char #x031F))
                            (#\% #.(code-char #x033E))
                            (#\- #.(code-char #x0320))))
                  (substitute #\_ #\- name))
              name))
      (let ((js-name (funcall *standard-symbol-to-js*
                       (if pkg
                           (intern bare-name pkg)
                           (make-symbol bare-name))
                       mangle)))
        (if modifier
            (concatenate 'string js-name (string modifier))
            js-name)))))

;; Booleans

(defun+ps true-p (thing)
  "Return true iff THING is the boolean true."
  (eq thing t))

(defun false-p (thing)
  "Return true iff THING is the boolean false."
  (eq thing nil))

(defpsfun false-p (thing)
  "Return true iff THING is the boolean false."
  (or (eq thing false)
      (eq thing null)
      (eq thing undefined)))

(defmacro or* (expr0 &rest exprs)
  "Same as OR."
  `(or ,expr0 ,@exprs))

(defpsmacro or* (expr0 &rest exprs)
  "Same as OR, except that non-boolean falses are considered true."
  (if (null exprs) expr0
      (with-var-value (expr0)
        `(if (false-p ,expr0) (or* ,@exprs) ,expr0))))

(defmacro if* (cond &rest branches)
  "Same as IF."
  `(if ,cond ,@branches))

(defpsmacro if* (cond &rest branches)
  "Same as IF, except that non-boolean falses are considered true."
  `(if (not (false-p ,cond)) ,@branches))


;; Numbers

(defun ensure-integer (str)
  "Given a string STR, try to parse it as a decimal integer."
  (parse-integer str :junk-allowed t))

(defpsfun ensure-integer (str)
  "Given a string STR, try to parse it as a decimal integer."
  (let ((n (parse-int str)))
    (if (is-finite n) n nil)))

(defpsmacro logandc2 (x y)
  "Return bitwise and X with the bitwise not of Y."
  `(logand ,x (lognot ,y)))

(defpsmacro integerp (x)
  "Return true if X is an integer, and false otherwise."
  (with-var-value (x)
    `(and (numberp ,x) (= (rem ,x 1) 0))))

(defpsmacro truncate (x y)
  "Divide X by Y, returning the integer quotient."
  (with-var-value (x)
    (with-var-value (y)
      `(~~ (/ ,x ,y)))))

(defpsmacro mod (x y)
  "Return X modulo Y, with the same sign as Y."
  (with-var-value (x)
    (with-var-value (y)
      `(rem (+ ,y (rem ,x ,y)) ,y))))


;; Strings and characters

(defun char-at (s position)
  "Return the character of string S at offset POSITION."
  (aref s position))

(defpsmacro char-at (s position)
  "Return a string containing the character at POSITION in S."
  `((@ ,s char-at) ,position))

(defun substring (s start &optional end)
  "Copy and return the portion of string S starting with element number
   START and continuing to the end of S or the optional END."
  (if end (subseq s start end) (subseq s start)))

(defpsmacro substring (s start &optional end)
  "Copy and return the portion of string S starting with element number
   START and continuing to the end of S or the optional END."
  `((@ ,s substring) ,start ,(or end 'undefined)))

(defpsmacro char-code (c)
  "Return the code unit value of the character represented by the string
   C (assumed to be of length 1)."
  `((@ ,c char-code-at) 0))

(defpsmacro code-char (i)
  "Return a string containing 1 character with code unit value I."
  `(chain *string (from-char-code ,i)))

(defpsmacro string (symbol-or-char)
  "Return SYMBOL-OR-CHAR verbatim (PS/JS represents symbols and characters
   as strings, so no conversion is needed)."
  symbol-or-char)

(defun string-designator-p (thing)
  "Return true iff THING is a valid argument to STRING."
  (or (stringp thing) (symbolp thing) (characterp thing)))

(defpsmacro string-designator-p (thing)
  "Return true iff THING is a valid argument to STRING."
  `(stringp ,thing))

(defun ensure-string (thing)
  "Coerce THING (normally, a vector of characters) to string."
  (or (ignore-errors (coerce thing 'string))
      (if (arrayp thing)
          (with-output-to-string (out)
            (loop for sub :across thing
                  do (format out "~A" (ensure-string sub))))
          (format nil "~A" thing))))

(defpsfun ensure-string (thing)
  "If THING is a vector (normally, of one-character strings), convert join
   it with empty separators to form a string.  Otherwise, do the standard
   string conversion."
  (cond
    ((stringp thing) thing)
    ((vectorp thing) ((@ thing join) ""))
    ((null thing) "")
    (t ((@ thing to-string)))))

(defun upcase (string-or-char)
  "Return the uppercase equivalent of the argument."
  (if (characterp string-or-char)
      (char-upcase string-or-char)
      (string-upcase string-or-char)))

(defpsmacro upcase (string)
  "Return the uppercase equivalent of the argument."
  `((@ ,string to-upper-case)))

(defun downcase (string-or-char)
  "Return the lowercase equivalent of the argument."
  (if (characterp string-or-char)
      (char-downcase string-or-char)
      (string-downcase string-or-char)))

(defpsmacro downcase (string)
  "Return the lowercase equivalent of the argument."
  `((@ ,string to-lower-case)))

(defmacro+ps interpolate (template)
  "Construct string from TEMPLATE which may contain interpolations of the
   form #EXPR and #{EXPR} (where EXPR is a Parenscript expression)."
  (with-input-from-string (stream template)
    (let ((*readtable* (copy-readtable)))
      (set-syntax-from-char #\{ #\()
      (set-syntax-from-char #\} #\))
      (set-syntax-from-char #\# #\')
      (loop
        for c := (read-char stream nil nil)
        for cc := nil
        with literal := nil
        when (and (or (not c)
                      (and (eql c #\#)
                           (not (eql (setq cc (peek-char nil stream))
                                     #\#))))
                  literal)
          collect (get-output-stream-string literal) :into pieces
          and do (setq literal nil)
        if (not c)
          return (cond
                   ((null pieces) "")
                   ((null (cdr pieces)) (car pieces))
                   (t `(concatenate 'string ,@pieces)))
        else if (or (not (eql c #\#))
                    (and (eql cc #\#) (setq c (read-char stream))))
          do (write-char c
               (or literal (setq literal (make-string-output-stream))))
        else
          do (setq cc (and (eql cc #\{) (read-char stream) #\}))
          and collect `(ensure-string ,(read stream)) :into pieces
          and if (eql cc #\})
            do (assert (eql (peek-char t stream) #\}))
               (read-char stream)))))


;; Vectors

(defpsmacro vector (&rest elts)
  "Create an array containing the given elements."
  `(array ,@elts))

(defpsmacro vectorp (thing)
  "Return true if THING is an array, and false otherwise."
  `(equal ((@ *object prototype to-string call) ,thing) "[object Array]"))

(defun ensure-vector (thing)
  "Return an array containing either THING or its contents."
  (if (typep thing 'sequence)
      (coerce thing 'vector)
      (vector thing)))

(defpsfun ensure-vector (thing)
  "Return an array containing either THING or its contents."
  (cond
    ((vectorp thing) thing)
    ((stringp thing) ((@ thing split) ""))
    ((null thing) (vector))
    (t (vector thing))))

(defun spliced (vector start del-count &rest elts)
  "Return a copy of VECTOR with DEL-COUNT elements beginning at index
   START replaced by ELTS.  If there are no ELTS, DEL-COUNT elements are
   just removed from the copy.  If DEL-COUNT is 0, ELTS are just inserted
   into it."
  (if (and (zerop del-count) (null elts)) vector
      (let* ((vlen (length vector))
             (new-vector (make-array
                          (+ vlen (max (- del-count) 0) (length elts))
                          :fill-pointer 0)))
        (loop for i :from 0 :below (min start vlen)
              do (vector-push (aref vector i) new-vector))
        (loop for x :in elts
              do (vector-push x new-vector))
        (loop for i :from (min (+ start del-count) vlen) :below vlen
              do (vector-push (aref vector i) new-vector))
        new-vector)))

(defpsfun spliced (vector start del-count &rest elts)
  "Return a copy of VECTOR with DEL-COUNT elements beginning at index
   START replaced by ELTS.  If there are no ELTS, DEL-COUNT elements are
   just removed from the copy.  If DEL-COUNT is 0, ELTS are just inserted
   into it."
  (let ((new-vector ((@ vector slice) 0)))
    (apply (@ new-vector splice) start del-count elts)
    new-vector))

(defun sliced (vector start &optional end)
  "Copy and return the portion of VECTOR between START and END."
  (if end (subseq vector start end) (subseq vector start)))

(defpsmacro sliced (vector start &optional end)
  "Copy and return the portion of VECTOR between START and END."
  `((@ ,vector slice) ,@(if end (list start end) (list start))))

(defpsfun reverse (vector)
  "Return a new array containing the same elements but in reverse order."
  ((@ (sliced vector 0) reverse)))

(defpsmacro member (thing seq &key test)
  "Return true iff THING is in SEQ.  If TEST is given, use it as the
   comparison function."
  (if test
      (with-var-value (seq)
        (with-var-value (test)
          (with-ps-gensyms (elt)
            `(loop for ,elt :across ,seq
                   if (funcall ,test ,thing ,elt)
                     return ,elt))))
      (with-ps-gensyms (i)
        `(let ((,i ((@ ,seq index-of) ,thing)))
           (and (>= ,i 0) (aref ,seq ,i))))))

(defpsmacro endp (seq)
  `(= (length ,seq) 0))

(defun vector-add (vec0 &rest vectors)
  "Return a new array of all the argument arrays concatenated together."
  (apply #'concatenate 'vector vec0 vectors))

(defpsmacro vector-add (vec0 &rest vectors)
  "Return a new array of all the argument arrays concatenated together."
  (flet ((ensure-vector (vec)
           (if (and (listp vec) (eq (car vec) 'vector)) vec
               `(or ,vec #()))))
    `(chain ,(ensure-vector vec0)
            (concat ,@(mapcar #'ensure-vector vectors)))))

(defmacro+ps aref* (vector offset)
  "Same as AREF, but performs bound checks, and allows negative
   offsets (from the end of the array)."
  (if (and (numberp offset) (minusp offset))
      (with-var-value (vector)
        `(and (<= ,(- offset) (length ,vector))
              (aref ,vector (+ (length ,vector) ,offset))))
      (with-var-value (vector)
        (with-var-value (offset)
          `(and (< ,offset (length ,vector))
                (aref ,vector ,offset))))))

(defun+ps vector-equal (a b test-fn)
  "Return true iff the vectors A and B have the same length and contain the
   same elements (under TEST-FN) in the same order."
  (and (= (length a) (length b))
       (loop for elt-a :across a
             for elt-b :across b
             if (not (funcall test-fn elt-a elt-b)) return nil
             finally (return t))))


;; Stacks, implemented as adjustable vectors in Lisp, as arrays in JS.

(declaim
 (inline make-stack stack-depth stack-empty-p stack-push stack-pop
         stack-peek stack-clear))

(defun make-stack ()
  "Create a new empty stack."
  (make-array 7 :fill-pointer 0 :adjustable t))

(defpsfun make-stack ()
  "Create a new empty stack."
  (make-array))

(defun stack (&rest elts)
  "Create a new stack pre-populated with ELTS (from bottom to top)."
  (let ((len (length elts)))
    (make-array len
      :initial-contents elts :fill-pointer len :adjustable t)))

(defpsfun stack (&rest elts)
  "Create a new stack pre-populated with ELTS (from bottom to top)."
  elts)

(defun stack-p (thing)
  "Return true if THING may be used as a stack, and NIL otherwise." 
  (and (vectorp thing)
       (adjustable-array-p thing)
       (array-has-fill-pointer-p thing)))

(defpsfun stack-p (thing)
  "Return true if THING may be used as a stack, and false otherwise." 
  (vectorp thing))

(defun stack-depth (stack)
  "The number of elements in STACK."
  (fill-pointer stack))

(defpsfun stack-depth (stack)
  "The number of elements in STACK."
  (length stack))

(defun+ps stack-empty-p (stack)
  "Return true if and only if there are no elements in STACK."
  (= 0 (stack-depth stack)))

(defun stack-push (thing stack)
  "Push THING onto STACK."
  (vector-push-extend thing stack))

(defpsfun stack-push (thing stack)
  "Push THING onto STACK."
  ((@ stack push) thing))

(defun stack-pop (stack &optional underflow-fn)
  "Remove the top element of STACK, returning it.  If STACK is empty,
   return the result of calling UNDERFLOW-FN."
  (if (plusp (fill-pointer stack))
      (vector-pop stack)
      (and underflow-fn (funcall underflow-fn))))

(defpsfun stack-pop (stack &optional underflow-fn)
  "Remove the top element of STACK, returning it.  If STACK is empty,
   return the result of calling UNDERFLOW-FN."
  (if (> (length stack) 0)
      ((@ stack pop))
      (and underflow-fn (funcall underflow-fn))))

(defun stack-peek (stack &optional (depth 1) underflow-fn)
  "Return the element of STACK which is at the given DEPTH (the top element
   is at depth 1).  If STACK has less than DEPTH elements, return the result
   of calling UNDERFLOW-FN."
  (let ((offset (- (fill-pointer stack) depth)))
    (if (minusp offset)
        (and underflow-fn (funcall underflow-fn))
        (aref stack offset))))

(defpsfun stack-peek (stack &optional (depth 1) underflow-fn)
  "Return the element of STACK which is at the given DEPTH (the top element
   is at depth 1).  If STACK has less than DEPTH elements, return the result
   of calling UNDERFLOW-FN."
  (let ((offset (- (length stack) depth)))
    (if (< offset 0)
        (and underflow-fn (funcall underflow-fn))
        (aref stack offset))))

(defun stack-clear (stack &optional scope)
  "Remove the top SCOPE elements (all elements, if N is not given) from
   STACK."
  (setf (fill-pointer stack)
        (if scope (max 0 (- (fill-pointer stack) scope)) 0)))

(defpsfun stack-clear (stack &optional scope)
  "Remove the top SCOPE elements (all elements, if N is not given) from
   STACK."
  (if (numberp scope)
      ((@ stack splice) (max 0 (- (length stack) scope)) scope)
      ((@ stack splice) 0 (length stack))))

(defun+ps stack-rotate (stack scope value &optional underflow-fn)
  "Rotate by VALUE positions the top SCOPE elements of STACK.  If STACK has
   less than SCOPE elements, return the result of calling UNDERFLOW-FN."
  (let ((depth (stack-depth stack)))
    (cond
      ((<= scope 0)
       nil)
      ((> scope depth)
       (and underflow-fn (funcall underflow-fn)))
      (t (dotimes (i scope)
           (stack-push
             (aref stack (+ (mod (- i value) scope) (- depth scope)))
             stack))
         (dotimes (i scope)
           (setf (aref stack (+ i (- depth scope)))
                   (aref stack (+ depth i))))
         (stack-clear stack scope)))))

(defun+ps copy-stack (thing)
  "Copy THING to a newly created stack.  If THING is an array it is copied
   elementwise, if not, as single element."
  (let ((new-stack (make-stack)))
    (if (vectorp thing)
        (loop for x :across thing
              do (stack-push x new-stack))
        (stack-push thing new-stack))
    new-stack))


;; Lookup tables, implemented as hash tables in Lisp, as objects in JS.

(defun make-table (&rest data)
  "Create a new lookup table and initialize it with DATA."
  (loop with table := (make-hash-table :test #'equal)
        for (key datum) :on data :by #'cddr
        do (setf (gethash key table) datum)
        finally (return table)))

(defpsmacro make-table (&rest data)
  "Create a new lookup table and initialize it with DATA."
  `(create ,@data))

(defun tablep (thing)
  "Return true iff THING is a table."
  (hash-table-p thing))

(defpsmacro tablep (thing)
  "Return true iff THING is a table."
  (with-var-value (thing)
    `(and ,thing (objectp ,thing))))

(defun copy-table (table)
  "Make a fresh copy of TABLE."
  (loop with copy := (make-hash-table :test #'equal)
        for key :being each hash-key of table :using (hash-value datum)
        do (setf (gethash key copy) datum)
        finally (return copy)))

(defpsmacro copy-table (table)
  "Make a fresh copy of TABLE."
  `(copy-structure ,table))

(defun lookup (key table &optional (default nil))
  "Look up KEY in TABLE and return the corresponding value, or DEFAULT if
   KEY is not defined."
  (gethash key table default))

(defpsmacro lookup (key table &optional (default nil defaultp))
  "Look up KEY in TABLE and return the corresponding value, or DEFAULT if
   KEY is not defined."
  (if defaultp
      `(let ((ret (getprop ,table ,key)))
         (if (eq ret undefined) ,default ret))
      `(getprop ,table ,key)))

(defun remember (key table value)
  "Associate KEY with VALUE in TABLE."
  (setf (gethash key table) value))

(defpsmacro remember (key table value)
  "Associate KEY with VALUE in TABLE."
  `(setf (getprop ,table ,key) ,value))

(defun forget (key table)
  "Remove KEY with its associated value from TABLE."
  (remhash key table))

(defpsmacro forget (key table)
  "Remove KEY with its associated value from TABLE."
  `(delete (getprop ,table ,key)))

(defun table-equal (a b test-fn)
  "Return true iff the tables A and B have the same number of entries, and
   every pair of entries with equal keys in A and B store values which are
   the same under TEST-FN."
  (and (= (hash-table-count a) (hash-table-count b))
       (loop for k :being each hash-key :of a
             using (hash-value va)
             always (multiple-value-bind (vb b-has-k) (gethash k b)
                      (and b-has-k (funcall test-fn va vb))))))

(defpsfun table-equal (a b test-fn)
  "Return true iff the tables A and B have the same number of entries, and
   every pair of entries with equal keys in A and B store values which are
   the same under TEST-FN."
  (let ((count 0))
    (for-in (prop a)
      (when ((@ a has-own-property) prop)
        (if (and ((@ b has-own-property) prop)
                 (funcall test-fn (getprop a prop) (getprop b prop)))
            (incf count)
            (return-from table-equal nil))))
    (for-in (prop b)
      (when ((@ b has-own-property) prop)
        (decf count)))
    (= count 0)))

(defun map-table (fn table)
  "Map function FN over the entries in TABLE."
  (maphash fn table))

(defpsfun map-table (fn table)
  "Map function FN over the entries in TABLE."
  (for-in (prop table)
    (when ((@ table has-own-property) prop)
      (funcall fn prop (getprop table prop)))))

;; Data templates

(defmacro js-quote (data)
  "JS-QUOTE and JS-UNQUOTE (on Lisp side) are equivalent to backquote-comma."
  (labels ((transform (thing)
             (cond
               ((consp thing)
                (if (eq (car thing) 'js-unquote)
                    (progn
                      (assert
                        (and (not (null (cdr thing))) (null (cddr thing))))
                      (cadr thing))
                    `(cons ,(transform (car thing))
                           ,(transform (cdr thing)))))
               ((and (vectorp thing) (not (stringp thing)))
                `(vector ,@(map 'list #'transform thing)))
               (t `(quote ,thing)))))
    (transform data)))

(defpsmacro js-quote (data)
  "JS-QUOTE and JS-UNQUOTE (on JavaScript side) are equivalent to
   backquote-comma, except that association lists serve as templates for
   JavaScript objects."
  (labels ((transform (thing)
             (cond
               ((consp thing)
                (cond
                  ((eq (car thing) 'js-unquote)
                    (progn
                      (assert
                        (and (not (null (cdr thing))) (null (cddr thing))))
                      (cadr thing)))
                  ((loop for elt :in thing
                         always (and (consp elt)
                                     (or (symbolp (car elt))
                                         (not (listp (cdr elt))))))
                   `(create
                     ,@(loop for elt :in thing
                             collect (car elt)
                             collect (transform (cdr elt)))))
                  (t `(list ,@(map 'list #'transform thing)))))
               ((and (vectorp thing) (not (stringp thing)))
                `(vector ,@(map 'list #'transform thing)))
               (t `(quote ,thing)))))
    (transform data)))


;; Objects

;; The use of a DEFSTRUCT form in ParenScript should cause to be defined
;; the same constructor, predicate, copier, and member accessors as in
;; Lisp (“same” here means that they have the same names and compatible
;; parameter-passing conventions).  Additionally, a MAKE-STRUCT-PS and a
;; MAKE-LOAD-FORM should be defined as Lisp methods for structures of
;; this type.
;; For conciseness' sake, the following definition omits the many
;; features of Lisp DEFSTRUCT which we don't use in this program.

(defgeneric make-struct-ps (struct)
  (:documentation "Return a form which may be used in PS code to represent
    STRUCT as literal.")
  (:method ((list list)) `(list ,@(mapcar #'make-struct-ps list)))
  (:method ((str string)) str)
  (:method ((vec vector)) `(vector ,@(map 'list #'make-struct-ps vec)))
  (:method ((null null)) 'nil)
  (:method (no-struct) no-struct))

(defmacro struct (thing)
  "Drop through to Lisp's built-in means of representing structure
   literals."
  `(the (or structure-object vector) ,thing))

(defpsmacro struct (thing)
  "Wrapper for MAKE-STRUCT-PS."
  `(lisp (make-struct-ps ,thing)))

(defun external-p (symbol)
  (eq (nth-value 1 (find-symbol (symbol-name symbol) '#:mex)) :external))

(defpsmacro defstruct (name-and-options &rest slots)
  "Define macros and functions to create instances of the specified
   structure, access its members, etc."
  (let (name super conc)
    (if (symbolp name-and-options)
        (setf name name-and-options
              conc (format nil "~A-" name))
        (loop initially (setf name (car name-and-options)
                              conc (format nil "~A-" name))
              for option :in (cdr name-and-options)
              do (destructuring-bind (op &optional arg)
                     (if (symbolp option) (list option) option)
                   (ecase op
                     (:conc-name (setf conc (and arg (string arg))))
                     (:include (setf super arg))))))
    ;; Discard documentation string.
    (when (stringp (car slots)) (pop slots))
    (let ((*name (intern (format nil "*~a" name)))
          (constructor (intern (format nil "MAKE-~a" name)))
          (predicate (intern (format nil "~a-P" name)))
          (accessors
           (loop for slot :in slots
                 collect (if conc
                             (intern (format nil "~a~a" conc slot))
                             slot))))
      `(progn
         (defun ,*name (,@slots)
           ,(format nil "Create a new instance of ~A." name)
           ,@(loop for slot :in slots
                   collect `(setf (@ this ,slot) ,slot))
           nil)
         ,(when super
            (let ((*super (intern (format nil "*~a" super))))
              `(setf (@ ,*name prototype) (new (,*super))
                     (@ (@ ,*name prototype) constructor) ,*name)))
         (lisp*
          (defmethod make-struct-ps ((struct ,name))
            (let ((obj ',*name)
                  (init-list (list
                              ,@(loop for accessor :in accessors
                                      collect `(make-struct-ps
                                                (,accessor struct))))))
              `(new (,obj ,@init-list)))))
         (lisp*
          (defmethod make-load-form ((struct ,name) &optional environment)
            (declare (ignorable environment))
            (list ',constructor
                  ,@(loop
                      for slot :in slots
                      for kwd := (intern (symbol-name slot) '#:keyword)
                      for accessor :in accessors
                      nconc (list kwd `(,accessor struct))))))
         (defmacro ,constructor (&rest init-list)
           (let ((obj ',*name)
                 (instance (ps-gensym (format nil "~a-" ',name))))
             `(let ((,instance (new (,obj))))
                ,@(loop for (kwd value) :on init-list :by #'cddr
                        for slot = (intern (symbol-name kwd))
                        collect `(setf (@ ,instance ,slot) ,value))
                ,instance)))
         ,(when (external-p constructor)
            `(defun ,constructor () (,constructor)))
         (defun ,predicate (thing)
           (and (objectp thing) (instanceof thing ,*name)))
         ,@(loop for slot :in slots
                 for accessor :in accessors
                 collect
                   `(defmacro ,accessor (instance)
                      (let ((slot-name ',slot))
                        `(@ ,instance ',slot-name)))
                 collect
                   `(defsetf ,accessor (instance) (value)
                      (let ((slot-name ',slot))
                        `(setf (@ ,instance ',slot-name) ,value)))
                 if (external-p accessor) collect
                   `(defun ,accessor (instance)
                      (,accessor instance)))))))

(defpsfun copy-structure (orig)
  "Create a shallow copy of the object ORIG."
  (let ((copy (new ((@ orig constructor)))))
     (for-in (prop orig)
       (when ((@ orig has-own-property) prop)
         (setf (getprop copy prop) (getprop orig prop))))
     copy))


;; Methods specialized to object types (of the first argument only)

(defun just-arg-names (lambda-list)
  "Return LAMBDA-LIST sans keywords."
  (loop for arg :in lambda-list
        unless (member arg lambda-list-keywords)
          if (listp arg)
            if (listp (setf arg (car arg)))
              do (setf arg (cadr arg))
            end
          end
          and collect arg))

(defun just-body (body)
  "Return BODY sans docstring and declarations."
  (loop for body* :on body
        for (expr) := body*
        while (or (stringp expr)
                  (and (consp expr) (eq 'declare (car expr))))
        finally (return body*)))

(defpsmacro defgeneric (name (obj &rest other-args) &rest methods-etc)
  "Define a function named NAME which tries to apply the corresponding
   method of its first argument OBJ to OTHER-ARGS.  If OBJ is no object or
   has no such method, run the default code which may be specified as the
   method for OBJ of type T."
  (let* ((plain-args (just-arg-names other-args))
         (default-method
          (loop
            for (kwd . def) :in methods-etc
            thereis (and (eq kwd :method)
                         (let ((ob (caar def)))
                           (and (or (symbolp ob)
                                    (and (listp ob) (eq (second ob) t)))
                                def)))
            finally
              (return
                `((,obj ,@other-args)
                  (let ((args
                         (list
                          ,@(loop for arg :in plain-args
                                  collect `(list (quote ,arg) ,arg)))))
                    (no-applicable-method (quote ,name) ,obj args))))))
         (default-method-expr
          (destructuring-bind ((ob &rest others) &body body)
              default-method
            (let ((renames
                   (loop for g-arg :in plain-args
                         for m-arg :in (just-arg-names others)
                         unless (eq g-arg m-arg)
                           collect `(,m-arg ,g-arg) :into renames
                         finally
                           (return
                             (if (eq ob obj) renames
                                 (cons `(,ob ,obj) renames))))))
              (if renames
                  `(let (,@renames) ,@(just-body body))
                  `(progn ,@(just-body body))))))
         (docstring
          (loop
            for (kwd def) :in methods-etc
            thereis (and (eq kwd :documentation) def))))
    `(progn
       (defun ,name (,obj ,@other-args)
         ,(or docstring
              (format nil "Try to call method ~A of ~A." name obj))
         (if (and ,obj (objectp ,obj) (functionp (@ ,obj ,name)))
             ((@ ,obj ,name) ,@plain-args)
             ,default-method-expr))
       ,@(loop for (kwd . def) :in methods-etc
               when (and (eq kwd :method) (not (eq def default-method)))
                 collect `(defmethod ,name ,@def)))))

(defpsmacro defmethod (name ((obj obj-type) &rest other-args) &body body)
  "Add a method named NAME with the given BODY to the prototype of
   OBJ-TYPE."
  (let ((constructor
         (case obj-type
           ((string symbol) '*string)
           ((array) '*array)
           ((boolean) '*boolean)
           ((number) '*number)
           (t (intern (format nil "*~a" obj-type))))))
    `(setf (@ ,constructor prototype ,name)
           (lambda ,(just-arg-names other-args)
             (let ((,obj this)) ,@(just-body body))))))


;; Syntax extensions

(defmacro define-enum-type-p (name start end)
  "Define a type predicate for integer enumeration type NAME."
  (let ((p-name (intern (format nil "~A-P" name))))
    `(defun ,p-name (thing)
       (typep thing '(integer ,start ,end)))))

(defpsmacro define-enum-type-p (name start end)
  "Define a type predicate for integer enumeration type NAME."
  (let ((p-name (intern (format nil "~A-P" name))))
    `(defun ,p-name (thing)
       (and (integerp thing)
            (<= ,start thing ,end)))))

(defmacro+ps defenum (name (&key (start 0)) &rest vars)
  "Define several VARS as successive values of an enumeration."
  (let ((end nil))
    `(progn
       ,@(loop for var :in vars
               for i :upfrom start
               collect `(defvar ,var ,i)
               finally (setq end i))
       (define-enum-type-p ,name ,start ,end))))

(defmacro+ps bind/init (((var location init) &rest more-bindings)
                        &body body)
  "Run BODY in the lexical environment where each VAR is bound to the value
   stored at the corresponding LOCATION (both VAR and LOCATION are
   initialized with the corresponding INIT if the value is null).  Return
   the result of the last expression in BODY."
  `(let ((,var ,location))
     (when (null ,var)
       (setf ,var ,init ,location ,var))
     ,@(if more-bindings
           `((bind/init ,more-bindings ,@body))
           body)))

(defmacro destructure/or (datum &rest clauses)
  "For use in macro definitions.  Try to destructure DATUM according
   to patterns that are CARs of CLAUSES, and evaluate respective
   clause bodies until we get a non-null value."
  `(or ,@(loop for (pattern . body) :in clauses
               if (eq pattern t)
                 collect `(progn ,@body)
               else
                 collect `(ignore-errors
                            (destructuring-bind ,pattern ,datum
                              ,@body)))))

;;; Local Variables: ***
;;; mode:lisp ***
;;; local-font-lock-keywords:(("(\\(def\\(?:ps\\(?:macro\\|fun\\)\\|\\(?:un\\|macro\\)\\+ps\\)\\)[ \t\n]+\\([^()]*?\\)[ \t\n]" (1 font-lock-keyword-face)) ("(\\(ambi-ps\\)\\_>" (1 font-lock-preprocessor-face))) ***
;;; End: ***
