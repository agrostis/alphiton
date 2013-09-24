(in-package #:mex)

;;; Groups

(ambi-ps ()

  ;; 

  (defstruct-guarded (group (:include opaque-context) (:guard context))
    "A sequence of tokens delimited by braces, which also constitutes a
     context with its own category, command and alias tables, effective
     while it is being parsed."
    start end lbrace contents rbrace token-count)

  (defmethod input-start ((grp group))
    (group-start grp))

  (defmethod input-end ((grp group))
    (group-end grp))

  (defmethod input-string ((ed error-display))
    nil)

  (defmethod input-string ((grp group))
    (loop for sub :across (group-contents grp)
          for sub-str := (input-string sub)
          while sub-str
          for str := sub-str :then (concatenate 'string str sub-str)
          finally (return str)))

  (declaim (ftype function get-group-contents mex-dispatch))

  (defvar *group-end-p* #'eot-p
    "Signifier for a predicate function which returns true iff its (single)
     argument signifies the end of a group.")

  (defvar *verify-group-balance* t
    "True iff unbalanced left or right brace should be treated as an
     error.")

  (defun next-group (token-source context &optional expand ship)
    "From TOKEN-SOURCE, extract a group (or, in the degenerate case, a
     singleton token), assuming the character categories in CONTEXT.  When
     EXPAND is true, commands and aliases are expanded according to their
     definitions in CONTEXT (and new definitions inside the group).  When
     SHIP is not null, it should be a function.  It is then called twice on
     the group object: 1) after the left brace is consumed, and the group
     initialized; and 2) after the right brace is consumed, and the group
     finalized; it is also passed on to GET-GROUP-CONTENTS.  NEXT-GROUP
     returns a list with two elements: the group extracted, and the token
     source that results after all the tokens have been consumed."
    (let ((tok0 (next-token/shift token-source context expand)))
      ;; A group begins with a non-active character of base category
      ;; lbrace (left brace) and ends with the balanced non-active
      ;; character of base category rbrace (right brace).
      (cond
        ((lbrace-token-p tok0)
         (let ((grp (guarded-make-group
                      :lbrace tok0 
                      :start (token-start tok0)
                      :category-table (category-table (token-context tok0))
                      :parent-context context))
               (*group-end-p* #'rbrace-token-p))
           (when ship (funcall ship grp context))
           (destructuring-bind (contents rbrace tsrc+)
               (get-group-contents token-source grp expand ship)
             (list 
               (if (eot-p contents)
                   (error-display* :add-to contents
                                   :append-message "eotInGroup")
                   (progn
                     (setf (group-end grp)
                             (or (and rbrace (token-end rbrace))
                                 (let ((last (aref* contents -1)))
                                   (and last (input-end last)))
                                 (token-end tok0))
                           (group-contents grp)
                             contents
                           (group-rbrace grp)
                             rbrace)
                     (if ship
                         (funcall ship grp context)
                         (setf (group-token-count grp)
                                 (+ (loop for sub :across contents
                                          sum (if (group-p sub)
                                                  (group-token-count sub)
                                                  1))
                                    (if rbrace 2 1))))
                     grp))
               tsrc+))))
        ((and (rbrace-token-p tok0)
              (not (eq *group-end-p* #'rbrace-token-p))
              *verify-group-balance*)
         (list (error-display* "hangingRbrace" tok0) token-source))
        (t (list tok0 token-source)))))

  (defmacro next-group/shift (token-source-location context
                              &optional expand ship)
    "Call NEXT-GROUP, set TOKEN-SOURCE-LOCATION to the token source that
     results when all group tokens have been consumed, and return the group
     object."
    (with-ps-gensyms (grp tsrc+)
      `(destructuring-bind (,grp ,tsrc+)
           (next-group ,token-source-location ,context ,expand ,ship)
         (setf ,token-source-location ,tsrc+)
         ,grp)))

  (defun get-group-contents (token-source context expand ship)
    "Keep extracting groups and singleton tokens from TOKEN-SOURCE until we
     get a terminal, i. e. an object satisfying *GROUP-END-P*.  If EXPAND is
     true, aliases and commands are expanded.  If SHIP is not null, it
     should be a function.  It is then passed on to NEXT-GROUP, and called
     on every extracted object which is not a group.  GET-GROUP-CONTENTS
     returns a list with three elements: a vector of extracted objects up to
     but not including the terminal (except when SHIP is not null), the
     terminal separately, and the token source which results after the
     terminal has been consumed."
    (loop for sub := (next-group/shift token-source context expand ship)
          for noexpand := (not expand)
          for expanded := nil
          #|
          do (format *trace-output* "~&******** Parsed out ~S, at ~S in ~S~%"
                     sub position context)
             (format *trace-output* "~&**** SHIFT-CONTEXT = ~S~%" shift-context)
          |#
          if (special-command-p sub "noexpand")
            do (setq sub (next-token/shift token-source context nil)
                     noexpand t)
          else if (funcall *group-end-p* sub)
            return (list (and (not ship) (ensure-vector contents))
                         sub token-source)
          else if (eot-p sub)
            return (list (if *verify-group-balance* sub
                             (ensure-vector contents))
                         nil token-source)
          ;; Expand the expandable, ship the unexpandable (or add it
          ;; to group contents).
          unless noexpand
            do (destructuring-bind (expd tsrc+)
                   (mex-dispatch sub token-source context)
                 (setf expanded expd token-source tsrc+))
          unless expanded
            if ship
              do (unless (group-p sub) (funcall ship sub context))
            else
              collect sub :into contents))

  (defun get-group-tokens (token-source context)
    "Run GET-GROUP-CONTENTS with command expansion to produce a flat
     sequence of tokens (possibly including group delimiter braces and error
     displays).  Return a list with two values: the token sequence, and a
     boolean indicating if there were errors while processing the input."
    (let ((tokens (make-stack))
          (errors nil)
          (*group-end-p* #'eot-p)
          (*verify-group-balance* nil))
      (get-group-contents token-source context t
        (lambda (sub ctx)
          (declare (ignore ctx))
          (if (group-p sub)
              (let ((brace (if (group-end sub)
                               (group-rbrace sub)
                               (group-lbrace sub))))
                (when brace (stack-push brace tokens)))
              (unless (eot-p sub) (stack-push sub tokens)))
          (when (error-display-p sub) (setf errors t))))
      (list tokens errors)))

  (defun get-group-string (token-source context &optional braces)
    "Run GET-GROUP-CONTENTS with command expansion to produce a string
     representation of resulting tokens (including or excluding group
     delimiter braces, according to the flag BRACES).  Return a list with
     two values: the string, and a boolean indicating if there were errors
     while processing the input."
    (let ((str "")
          (errors nil)
          (*group-end-p* #'eot-p)
          (*verify-group-balance* nil))
      (get-group-contents token-source context t
        (lambda (sub ctx)
          (declare (ignore ctx))
          (when (and braces (group-p sub))
            (setf sub (if (group-end sub)
                          (group-rbrace sub)
                          (group-lbrace sub))))
          (cond
            ((and (token-p sub) (not (eot-token-p sub)))
             (setf str (concatenate 'string str (input-string sub))))
            ((error-display-p sub)
             (setf errors t)))))
      (list str errors)))

#|
  (defun tokens-after (source position context end-token)
    (loop for tok := (token-at/context-shift source position context)
          collect tok :into tokens
          if (or (eot-p tok) (token-context-equal tok end-token))
            return (ensure-vector tokens)))
|#

  (defun group-equal (a b)
    "Return true iff A and B are equal groups."
    (or (and (group-p a) (group-p b)
             (= (group-token-count a) (group-token-count b))
             (token-equal (group-lbrace a) (group-lbrace b))
             (token-equal (group-rbrace a) (group-rbrace b))
             (vector-equal (group-contents a) (group-contents b)
                           #'group-equal))
        (token-equal a b)))

)

(defmethod print-object ((group group) stream)
  (if *print-readably*
      (error 'print-not-readable :object group)
      (print-unreadable-object (group stream :type t)
        (format stream "[~S, ~S)~@[ :: ~S TOKENS~]"
                (group-start group) (group-end group)
                (group-token-count group)))))

;;; Local Variables: ***
;;; mode:lisp ***
;;; local-font-lock-keywords:(("(\\(def\\(?:\\(ps\\(?:macro\\|fun\\)\\|\\(?:un\\|macro\\)\\+ps\\)\\|\\(guard\\|enum\\)\\|\\(struct-guarded\\)\\)\\)[ \t\n]+(?\\([^()]*?\\)[ \t\n]" (1 font-lock-keyword-face) (5 (cond ((match-beginning 2) font-lock-function-name-face) ((match-beginning 3) font-lock-variable-name-face) ((match-beginning 4) font-lock-type-face)))) ("(\\(ambi-ps\\)\\_>" (1 font-lock-preprocessor-face))) ***
;;; End: ***
