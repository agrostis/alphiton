;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; To be removed as soon as the corresponding commits are merged into ;;;
;;; github.com/vsedach/Parenscript and propagate to Quicklisp          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:parenscript)
(in-readtable :parenscript)

;;; commit 5ca828dcc0aa43c0a52b17a3eaad05c44b1cbb12
;;; Author: Boris Smilga <boris.smilga@gmail.com>
;;; Date:   Wed Oct 17 02:09:26 2012 +0400
;;;
;;;     Method functions should be applied to their host objects as thisArgs, rather than to the callers' this.

(defpsmacro apply (fn &rest args)
  (let ((arglist (if (> (length args) 1)
                     `(append (list ,@(butlast args)) ,(car (last args)))
                     (first args))))
    (if (and (listp fn)
             (find (car fn) #(getprop chain @)))
        (if (and (= (length fn) 3) (symbolp (second fn)))
            `(funcall (getprop ,fn 'apply) ,(second fn) ,arglist)
            (let ((obj (ps-gensym)) (method (ps-gensym)))
              `(let* ((,obj    ,(butlast fn))
                      (,method (,(car fn) ,obj ,(car (last fn)))))
                 (funcall (getprop ,method 'apply) ,obj ,arglist))))
        `(funcall (getprop ,fn 'apply) this ,arglist))))

;;; commit 0cb1d2def80a1ee4f3017aff13610acc07899990
;;; Author: Boris Smilga <boris.smilga@gmail.com>
;;; Date:   Tue Nov 13 19:40:45 2012 +0400
;;;
;;;     Lisp-like initialization options for MAKE-ARRAY.

(defpsmacro make-array (&rest args)
  (or (ignore-errors
        (destructuring-bind (dim &key (initial-element nil initial-element-p)
                                 initial-contents element-type)
            args
          (declare (ignore element-type))
          (and (or initial-element-p initial-contents)
               (not (and initial-element-p initial-contents))
               (with-ps-gensyms (arr init elt i)
                 `(let ((,arr (new (*array ,dim))))
                    ,@(when initial-element-p
                        `((let ((,elt ,initial-element))
                            (dotimes (,i (length ,arr))
                              (setf (aref ,arr ,i) ,elt)))))
                    ,@(when initial-contents
                        `((let ((,init ,initial-contents))
                            (dotimes (,i (min (length ,arr) (length ,init)))
                              (setf (aref ,arr ,i) (aref ,init ,i))))))
                    ,arr)))))
      `(new (*array ,@args))))

;;; commit 5b1fa4344453b0dc668f3df7c95b97a02dc17074
;;; Author: Boris Smilga <boris.smilga@gmail.com>
;;; Date:   Wed Oct 1 00:46:10 2014 +0400
;;;
;;;     Fixed: RETURN-FROM a nested block in tail context should behave as RETURN-FROM the outer block.

(defun expressionize-result (tag form)
  (ps-compile
   (case (car form)
     ((continue break throw) ;; non-local exit
      form)
     ;; implicit progn forms
     ((with label let flet labels macrolet symbol-macrolet)
      `(,(first form) ,(second form)
         ,@(butlast (cddr form))
         (return-from ,tag ,(car (last (cddr form))))))
     (progn
       `(progn ,@(butlast (cdr form))
               (return-from ,tag ,(car (last (cdr form))))))
     (switch
      `(switch
        ,(second form)
        ,@(loop for (cvalue . cbody) in (cddr form)
                for remaining on (cddr form) collect
                (aif (cond ((or (eq 'default cvalue) (not (cdr remaining)))
                            1)
                           ((eq 'break (car (last cbody)))
                            2))
                     (let ((result-form (ps-macroexpand (car (last cbody it)))))
                       `(,cvalue
                         ,@(butlast cbody it)
                         (return-from ,tag
                           ,(if (eq result-form 'break) nil result-form))))
                     (cons cvalue cbody)))))
     (try
      `(try (return-from ,tag ,(second form))
            ,@(let ((catch (cdr (assoc :catch (cdr form))))
                    (finally (assoc :finally (cdr form))))
                   (list (when catch
                           `(:catch ,(car catch)
                              ,@(butlast (cdr catch))
                              (return-from ,tag ,(car (last (cdr catch))))))
                         finally))))
     (cond
       `(cond
          ,@(loop for clause in (cdr form) collect
                  `(,@(butlast clause) (return-from ,tag ,(car (last clause)))))
          ,@(when in-case? `((t (return-from ,tag nil))))))
     (if
      (if (and (try-expressionizing-if? form)
               (not (find 'values (flatten form)))
               (let ((used-up-names                   *used-up-names*)
                     (*lambda-wrappable-statements*                ()))
                 (handler-case (compile-expression form)
                   (compile-expression-error ()
                     (setf *used-up-names* used-up-names)
                     nil))))
          (return-from expressionize-result (return-exp tag form))
          `(if ,(second form)
               (return-from ,tag ,(third form))
               ,@(when (or in-case? (fourth form))
                       `((return-from ,tag ,(fourth form)))))))
     (block
      (let ((tag* (or (cadr form) 'nilBlock))
            (body* (cddr form)))
        (let ((*function-block-names* (cons tag* *function-block-names*)))
          (return-from expressionize-result
            (expressionize-result tag* `(progn ,@body*))))))
     (return-from ;; this will go away someday
      (unless tag
        (warn 'simple-style-warning
              :format-control "Trying to RETURN a RETURN without a block tag specified. Perhaps you're still returning values from functions by hand?
Parenscript now implements implicit return, update your code! Things like (lambda () (return x)) are not valid Common Lisp and may not be supported in future versions of Parenscript."))
       form)
     (otherwise
      (return-from expressionize-result
        (cond ((not (gethash (car form) *special-statement-operators*))
               (return-exp tag form))
              (in-case?
               `(ps-js:block ,(compile-statement form) ,(return-exp tag)))
              (t (compile-statement form))))))))


;;; commit 1a7617548a228020cbe75375223f6e693f56935f
;;; Author: Boris Smilga <boris.smilga@gmail.com>
;;; Date:   Thu Dec 18 15:05:36 2014 +0300
;;;
;;;     Fixed: empty-body clauses in COND should evaluate to the result of the test.

(define-expression-operator cond (&rest clauses)
  (compile-expression
   (when clauses
     (destructuring-bind (test &rest body) (car clauses)
       (if (eq t test)
           (if (null body) t `(progn ,@body))
           (flet ((conditional (test body)
                    `(if ,test
                         (progn ,@body)
                         (cond ,@(cdr clauses)))))
             (if (null body)
                 (with-ps-gensyms (test-result)
                   `(let ((,test-result ,test))
                      ,(conditional test-result (list test-result))))
                 (conditional test body))))))))

(define-statement-operator cond (&rest clauses)
  (let* ((test-result nil)
         (clauses*
           (loop for clause in clauses for (test . body) = clause
                 if body
                   collect clause
                 else
                   do (unless test-result (setq test-result (ps-gensym)))
                   and collect
                     (if (and (consp test) (eq (first test) 'return-from))
                         (cons `(setq ,test-result ,(third test))
                               `((return-from ,(second test) ,test-result)))
                         (cons `(setq ,test-result ,test)
                               `(,test-result)))))
         (if-form
           `(ps-js:if
              ,(compile-expression (caar clauses*))
              ,(compile-statement `(progn ,@(cdar clauses*)))
              ,@(loop for (test . body) in (cdr clauses*) appending
                  (if (eq t test)
                      `(:else ,(compile-statement `(progn ,@body)))
                      `(:else-if ,(compile-expression test)
                                 ,(compile-statement `(progn ,@body))))))))
    (if test-result
        `(ps-js:block (ps-js:var ,test-result) ,if-form)
        if-form)))


;;; commit cd81a4dfc247b44db29f03d98b63a0ed55d17111
;;; Author: Boris Smilga <boris.smilga@gmail.com>
;;; Date:   Mon Dec 15 21:59:55 2014 +0300
;;;
;;;     Added support for getters and setters in object literals.

(export 'ps-js::set '#:ps-js)

(define-expression-operator create (&rest arrows)
  `(ps-js:object
    ,@(loop for (key val-expr) on arrows by #'cddr
            for (accessor . accessor-args) =
              (when (and (consp key)
                         (symbolp (first  key))
                         (symbolp (second key)))
                (case (first key)
                  (get (and (null (third key))
                            `((ps-js::get ,(second key)))))
                  (set (and (symbolp (third key)) (null (fourth key))
                            `((ps-js::set ,(second key)) ,(third key))))))
            collecting
              (if accessor
                  (list accessor accessor-args
                        (let ((*function-block-names* ()))
                          (compile-function-body (third accessor)
                                                 (list val-expr))))
                  (cons (cond ((and (symbolp key) (reserved-symbol-p key))
                               (reserved-symbol-p key))
                              ((or (stringp key) (numberp key) (symbolp key))
                               key)
                              ((and (consp key)
                                    (eq 'quote (first  key))
                                    (symbolp   (second key))
                                    (null      (third  key)))
                               (symbol-to-js-string (second key)))
                              (t
                               (error
                                 "Slot key ~s is not one of symbol, string, ~
                                  number, or accessor spec."
                                 key)))
                        (compile-expression val-expr))))))

(defun print-fun-def (name args body)
  (destructuring-bind (keyword name) (if (consp name) name `(function ,name))
    (format *psw-stream* "~(~A~) ~:[~;~A~]("
            keyword name (symbol-to-js-string name))
    (loop for (arg . remaining) on args do
        (psw (symbol-to-js-string arg)) (when remaining (psw ", ")))
    (psw ") ")
    (ps-print body)))

(defprinter ps-js:object (&rest slot-defs)
  (parenthesize-at-toplevel
   (lambda ()
     (psw "{ ")
     (let ((indent? (< 2 (length slot-defs)))
           (indent *column*))
       (loop for ((slot-name . slot-value) . remaining) on slot-defs do
            (if (consp slot-name)
                (apply #'print-fun-def slot-name slot-value)
                (progn
                  (ps-print slot-name) (psw " : ")
                  (if (and (consp slot-value)
                           (eq 'ps-js:|,| (car slot-value)))
                      (parenthesize-print slot-value)
                      (ps-print slot-value))))
            (when remaining
              (psw ",")
              (if indent?
                  (newline-and-indent indent)
                  (psw #\Space))))
       (if indent?
           (newline-and-indent (- indent 2))
           (psw #\Space)))
     (psw "}"))))


(defmacro with-declaration-effects ((var block) &body body)
  (with-ps-gensyms (decls)
    `(multiple-value-bind (,decls ,var) (parse-body ,block)
       (let ((*special-variables*
               (append (loop for decl in ,decls
                             nconcing
                               (loop for (id . rest) in (cdr decl)
                                     if (eq id 'special)
                                       nconc rest))
                       *special-variables*)))
         ,@body))))


;;; 4cf5031602fdca26792574d0a2d86f85a3a9852a
;;; Author: Boris Smilga <boris.smilga@gmail.com>
;;; Date:   Thu Dec 25 22:35:44 2014 +0300
;;;
;;;     Improved handling of declarations.

(defmacro with-declaration-effects ((var block) &body body)
  (with-ps-gensyms (decls)
    `(multiple-value-bind (,decls ,var) (parse-body ,block)
       (let ((*special-variables*
               (append (loop for decl in ,decls
                             nconcing
                               (loop for (id . rest) in (cdr decl)
                                     if (eq id 'special)
                                       nconc rest))
                       *special-variables*)))
         ,@body))))

(defun parse-body (body &key allow-docstring)
  (flet ((values* (docstring declarations body)
           (if allow-docstring
               (values docstring declarations body)
               (values declarations body))))
    (loop for forms on body for (form) = forms
          with docstring = (not allow-docstring)
          if (and (stringp form) (not docstring))
            do (setf docstring form)
          else if (and (consp form) (eq (first form) 'declare))
            collect form into declarations
          else
            return (values* docstring declarations forms)
          finally (return (values* docstring declarations nil)))))

(defun parse-extended-function (lambda-list body)
  (multiple-value-bind (requireds optionals rest? rest keys? keys allow? aux?
                                  aux more? more-context more-count key-object)
      (parse-lambda-list lambda-list)
    (declare (ignore allow? aux? aux more? more-context more-count key-object))
    (let* ( ;; optionals are of form (var default-value)
           (effective-args
            (remove-if #'null
                       (append requireds
                               (mapcar #'parse-optional-spec optionals))))
           (opt-forms
            (mapcar (lambda (opt-spec)
                      (multiple-value-bind (name value suppl)
                          (parse-optional-spec opt-spec)
                        (cond (suppl
                               `(progn
                                  (var ,suppl (not (eql ,name undefined)))
                                  ,@(when value
                                          `((when (not ,suppl) (setf ,name ,value))))))
                              (value
                               `(when (eql ,name undefined)
                                  (setf ,name ,value))))))
                    optionals))
           (key-forms
            (when keys?
              (with-ps-gensyms (n)
                (let (defaults assigns)
                  (mapc
                   (lambda (k)
                     (multiple-value-bind (var init-form keyword-str suppl)
                         (parse-key-spec k)
                       (push `(var ,var ,@(when init-form `((if (undefined ,var) ,init-form ,var)))) defaults)
                       (when suppl (push `(var ,suppl) defaults))
                       (push `(,keyword-str
                               (setf ,var (aref arguments (1+ ,n))
                                     ,@(when suppl `(,suppl t))))
                             assigns)))
                   (reverse keys))
                  `((loop for ,n from ,(length requireds) below (length arguments) by 2 do
                          (case (aref arguments ,n)
                            ,@assigns))
                    ,@defaults)))))
           (rest-form
            (when rest?
              `(var ,rest
                    ((@ Array prototype slice call)
                     arguments ,(length effective-args))))))
      (multiple-value-bind (docstring declarations executable-body)
          (parse-body body :allow-docstring t)
        (values effective-args
                (append declarations
                        opt-forms key-forms (awhen rest-form (list it))
                        executable-body)
                docstring)))))

(defun compile-function-body (args body)
  (with-declaration-effects (body body)
    (let* ((in-function-scope?                          t)
           (*vars-needing-to-be-declared*              ())
           (*used-up-names*                            ())
           (*enclosing-function-arguments*
            (append args *enclosing-function-arguments*))
           (*enclosing-lexicals*
            (set-difference *enclosing-lexicals* args))
           (collapsed-body
            (collapse-function-return-blocks body))
           (suppress-values?
            (find 'values (flatten body)))
           (*dynamic-return-tags*
            (append (mapcar (lambda (x) (cons x nil))
                            *function-block-names*)
                    *dynamic-return-tags*))
           (body
            (let ((in-loop-scope?                 nil)
                  (*loop-scope-lexicals*           ())
                  (*loop-scope-lexicals-captured*  ()))
              (cdr
               (wrap-for-dynamic-return
                *function-block-names*
                (compile-statement
                 `(return-from %function (progn ,@collapsed-body)))))))
           (var-decls
            (compile-statement
             `(progn
                ,@(mapcar
                   (lambda (var)
                     `(var ,var))
                   (remove-duplicates *vars-needing-to-be-declared*))))))
      (when in-loop-scope? ;; this might be broken when it comes to let-renaming
        (setf *loop-scope-lexicals-captured*
              (append (intersection (flatten body) *loop-scope-lexicals*)
                      *loop-scope-lexicals-captured*)))
      `(ps-js:block ,@(reverse (cdr var-decls))
         ,@body))))

(define-expression-operator lambda (lambda-list &rest body)
  (multiple-value-bind (effective-args effective-body)
      (parse-extended-function lambda-list body)
    `(ps-js:lambda ,effective-args
       ,(let ((*function-block-names* ()))
          (compile-function-body effective-args effective-body)))))

(define-expression-operator let (bindings &body body)
  (with-declaration-effects (body body)
    (flet ((rename (x) (first x))
           (var (x) (second x))
           (val (x) (third x)))
      (let* ((new-lexicals ())
             (normalized-bindings
              (mapcar (lambda (x)
                        (if (symbolp x)
                            (list x nil)
                            (list (car x) (ps-macroexpand (cadr x)))))
                      bindings))
             (symbols-in-bindings
              (mapcan (lambda (x) (flatten (cadr x)))
                      normalized-bindings))
             (lexical-bindings
              (loop for x in normalized-bindings
                    unless (special-variable? (car x)) collect
                    (cons (aif (maybe-rename-lexical-var (car x)
                                                         symbols-in-bindings)
                               it
                               (progn
                                 (push (car x) new-lexicals)
                                 (when (boundp '*used-up-names*)
                                   (push (car x) *used-up-names*))
                                 nil))
                          x)))
             (dynamic-bindings
              (loop for x in normalized-bindings
                    when (special-variable? (car x)) collect
                    (cons (ps-gensym (format nil "~A_~A" (car x) 'tmp-stack))
                          x)))
             (renamed-body
              `(symbol-macrolet ,(loop for x in lexical-bindings
                                       when (rename x) collect
                                       `(,(var x) ,(rename x)))
                 ,@body))
             (*enclosing-lexicals*
              (append new-lexicals *enclosing-lexicals*))
             (*loop-scope-lexicals*
              (when in-loop-scope?
                (append new-lexicals *loop-scope-lexicals*)))
             (let-body
              `(progn
                 ,@(mapcar (lambda (x)
                             `(var ,(or (rename x) (var x)) ,(val x)))
                           lexical-bindings)
                 ,(if dynamic-bindings
                      `(progn
                         ,@(mapcar (lambda (x) `(var ,(rename x)))
                                   dynamic-bindings)
                         (try
                          (progn
                            (setf ,@(loop for x in dynamic-bindings append
                                         `(,(rename x) ,(var x)
                                            ,(var x) ,(val x))))
                            ,renamed-body)
                          (:finally
                           (setf ,@(mapcan (lambda (x) `(,(var x) ,(rename x)))
                                           dynamic-bindings)))))
                      renamed-body))))
        (ps-compile (cond ((or in-function-scope?
                               (null bindings))
                           let-body)
                          ;; HACK
                          ((find-if (lambda (x)
                                      (member x '(defun% defvar)))
                                    (flatten
                                     (loop for x in body collecting
                                          (or (ignore-errors (ps-macroexpand x))
                                              x))))
                           let-body)
                          (t
                           (with-lambda-scope let-body))))))))

(defun pop-declarations-for-var (var declarations)
  (loop for declarations* on declarations
        with var-declarations = nil
        do (setf (first declarations*)
                 (loop for spec in (first declarations*)
                       ;; We only care for SPECIAL declarations for now
                       ;; (cf. WITH-DECLARATION-EFFECTS)
                       if (and (consp spec) (eq 'special (first spec)))
                         collect
                           (let ((vars* (remove var (rest spec))))
                             (if (eq vars* (cdr spec))
                                 spec
                                 (progn
                                   (pushnew var (getf var-declarations 'special))
                                   (cons 'special vars*))))
                       else
                         collect spec))
        finally (return
                  (loop for (sym decls) on var-declarations by #'cddr
                        collect (cons sym decls)))))

(defun destructuring-wrap (arr n bindings declarations body)
  (cond ((null bindings) body)
        ((eq (car bindings) '&rest)
         (cond ((and (= (length bindings) 2) (atom (second bindings)))
                `(let ((,(second bindings) (if (> (length ,arr) ,n) ((@ ,arr slice) ,n) '())))
                   (declare ,@(pop-declarations-for-var (second bindings) declarations))
                   ,body))
               (t (error "~a is invalid in destructuring list." bindings))))
        ((eq (car bindings) '&optional)
         (destructuring-wrap arr n (cdr bindings) declarations body))
        (t (let ((var (car bindings))
                 (inner-body (destructuring-wrap arr (1+ n) (cdr bindings) declarations body)))
             (cond ((null var) inner-body)
                   ((atom var) `(let ((,var (aref ,arr ,n)))
                                  (declare ,@(pop-declarations-for-var var declarations))
                                  ,inner-body))
                   (t `(,'destructuring-bind ,var (aref ,arr ,n)
                         ,@declarations
                         ,inner-body)))))))

(defpsmacro destructuring-bind (bindings expr &body body)
  (setf bindings (dot->rest bindings))
  (multiple-value-bind (declarations executable-body) (parse-body body)
    (let* ((arr (if (hoist-expr? bindings expr) (ps-gensym "_DB") expr))
           (bound (destructuring-wrap arr 0 bindings declarations
                                      (cons 'progn executable-body))))
      (cond ((eq arr expr) bound)
            (t `(let ((,arr ,expr)) ,bound))))))

(defpsmacro let* (bindings &body body)
  (multiple-value-bind (declarations executive-body) (parse-body body)
    (loop for binding in (cons nil (reverse bindings))
          for var = (if (symbolp binding) binding (car binding))
          for body = executive-body
            then `((let (,binding)
                     (declare ,@(pop-declarations-for-var var declarations))
                     ,@body))
          finally (return `(progn ,@body)))))

;;; commit dcf452393bf51285b41f2439f049988d6f1ad74e
;;; Author: Boris Smilga <boris.smilga@gmail.com>
;;; Date:   Sun Jan 4 15:25:57 2015 +0300
;;;
;;;     Fixed unparenthesized comma-sequences in for(;;) variable initializer expressions inside blocks.

(defprinter ps-js:for (vars tests steps body-block)
  "for ("
  (loop for ((var-name . var-init) . remaining) on vars
     for decl = "var " then "" do
       (psw decl (symbol-to-js-string var-name) " = ")
       (print-op-argument 'ps-js:= var-init)
       (when remaining (psw ", ")))
  "; "
  (loop for (test . remaining) on tests do
       (ps-print test) (when remaining (psw ", ")))
  "; "
  (loop for (step . remaining) on steps do
       (ps-print step) (when remaining (psw ", ")))
  ") "
  (ps-print body-block))
