(in-package #:mex)

;;; Core Mex builtins

(ambi-ps ()

  (defbuiltin relax (match dispatching context)
    "Expand to nothing, with no side effects."
    (list t (match-token-source match)))

  (defbuiltin comment (match dispatching context)
    "Consume everything to the end of the line, expand to nothing."
    (loop with tsrc := (match-token-source match)
          until (if-match-bind ((tok token)) tsrc context
                  (or (newline-token-p tok) (eot-p tok)))
          finally (return (list t tsrc))))

  (defbuiltin expandafter (match dispatching context)
    "If the input after the first token (with parameters expanded) is a
     command, expand to the expansion of that command, preceded by the first
     token (the token and the following command are consumed).  Otherwise,
     expand to nothing, with no side effects."
    (let ((tsrc (match-token-source match)))
      (if-match-bind ((tok1 token) (tok2 token*)) tsrc context
        (destructuring-bind (expd tsrc+) (mex-dispatch tok2 tsrc context)
          (list t
                (if expd
                    (expansion-to-token-source (vector tok1) tsrc+ nil)
                    (match-token-source match)))))))

  (defun replace-context (match dispatching get-context)
    "Helper function for the builtins GLOBAL and LOCAL.  Consume one token
     after MATCH (using the native context of DISPATCHING).  The expansion
     is a copy of the token with its native context replaced by the value of
     the GET-CONTEXT thunk."
    (let ((tsrc (match-token-source match)))
      (if-match-bind ((tok token #'token-p))
          tsrc (token-context dispatching)
        (let ((context* (funcall get-context)))
          (if context*
              (let ((tok* (copy-structure tok)))
                (setf (token-context tok*) context*)
                (list (vector tok*) tsrc))
              (list (error-display* "noContextInput" dispatching tok) tsrc)))
        (list (error-display* "invalidToken" dispatching tok) tsrc))))

  (defbuiltin global (match dispatching context)
    "Consume one token.  Expand to a token which is just like the consumed
     but whose native context is the global (top-level) context."
    (flet ((global-ctx ()
             (loop for ctx := (ensure-opaque-context context)
                           :then (parent-context ctx)
                   with gctx := nil
                   while ctx
                   if (or *no-transparent-contexts* (opaque-context-p ctx))
                     do (setf gctx ctx)
                   finally (return gctx))))
      (replace-context match dispatching #'global-ctx)))

  (defbuiltin local (match dispatching context)
    "Consume one token.  Expand to a token which is just like the consumed
     but whose native context is the local context (group or top-level)."
    (replace-context match dispatching (lambda () context)))
                     
  (defbuiltin setlang (match dispatching context)
    "Consume one group (or singleton token), whose string representation
     should name a language.  Switch the locale of CONTEXT and all ancestor
     contexts to this language; expand to nothing."
    (let ((tsrc (match-token-source match)))
      (if-match-bind ((arg group)) tsrc context
        (let ((locale-id (input-string arg)))
          (if locale-id
              (progn
                (set-context-locale context locale-id)
                (list t tsrc))
              (list
                (error-display* :add-to arg :append-message "invalidLocale"
                                :replace-input dispatching arg)
                tsrc))))))

  (defbuiltin csname (match dispatching context)
    "Consume tokens up to the nearest occurence of \endcsname,
     expanding parameters.  Concatenate string representations of the
     tokens (not including \endcsname) into one string, and expand to
     a command token with that string for name."
    (let ((tsrc (match-token-source match)))
      (loop
        for tok := (next-token/expand tsrc context)
        if (or (par-break-p tok) (eot-p tok))
          return (list
                   (error-display* :add-to tok
                                   :append-message "eotBeforeExpected"
                                   :prepend-input dispatching content)
                   tsrc)
        else if (token-is tok :command "endcsname")
          return (list
                   (vector (make-command-token
                             :start (token-start dispatching)
                             :end (token-end tok)
                             :context (token-context dispatching)
                             :name (input-string (ensure-vector content))
                             :escape-chr #\\))
                   tsrc)
        else
          collect tok :into content)))

  (defun parse-definition (dispatching token-source context no-pattern-p)
    "Helper function for the builtins DEF, LCDEF and ALIAS.  Parse input
     from TOKEN-SOURCE for a macro / alias definition.  Return a list with
     three values: the command or active token for the defined name (or an
     error display, if the definition is syntactially invalid), a MACRO
     object linking the pattern and the expansion (or just the expansion if
     NO-PATTERN-P is true), and a token source pointing after the
     definition."
    (if-match-bind ((cmd-tok token #'dispatching-token-p))
        token-source context
      (loop named parse-def
        with delimiter := nil
        with param := nil
        with expn := nil
        do (setf delimiter nil param nil expn nil)
           (flet ((pattern-toks ()
                    (let ((pattern-toks (stack dispatching)))
                      (loop
                        for part :in pattern
                        if (token-p part)
                          do (stack-push part pattern-toks)
                        else if (pattern-delimiter-p part)
                          do (loop for tok :across (delimiter-tokens part)
                                   do (stack-push tok pattern-toks)))
                      (and (> (length pattern-toks) 0)
                           (ensure-vector pattern-toks)))))
             (loop
               with d-tok := nil 
               do (if-match-bind ((input group)) token-source context
                    (cond ((group-p input)
                           (setf expn (group-contents input)))
                          ((and no-pattern-p (not (eot-p input)))
                           (setf expn (vector input)))
                          ((param-token-p input)
                           (setf param input))
                          (t (setf d-tok input))))
               if (or param expn)
                 if d-tok
                   do (setf delimiter
                              (make-pattern-delimiter
                                :tokens (ensure-vector d-toks)))
                 end
                 and return t
               else if (or (funcall *group-end-p* d-tok)
                           (par-break-p d-tok)
                           (eot-p d-tok))
                 do (return-from parse-def
                      (list (error-display*
                              :add-to d-tok :append-message "unfinishedDef"
                              :prepend-input (pattern-toks))
                            nil token-source))
               else
                 collect d-tok :into d-toks))
        when delimiter
          collect delimiter :into pattern
        when param
          collect param :into pattern
        when expn
          return (list cmd-tok
                       (if no-pattern-p expn
                           (make-macro :pattern (ensure-vector pattern)
                                       :expansion expn))
                       token-source))
      (list (if (eot-p cmd-tok)
                (error-display* "unfinishedDef" dispatching)
                (error-display* "invalidCommandInDef" cmd-tok))
            nil token-source)))

  (defbuiltin def (match dispatching context)
    "Consume one command or active character token, followed by a (possibly
     empty) sequence of singleton tokens, followed by a group.  Use the
     first token to obtain a command key.  Create a macro by separating the
     sequence of singleton tokens into a sequence of alternating parameter
     tokens and delimiters of literal tokens, and using it as the pattern,
     and the contents of the group as the expansion.  Store the macro in the
     command table of the native context of the dispatching token (or its
     nearest opaque ancestor) under the given command key.  Expand to
     nothing."
    (destructuring-bind (cmd-tok-or-error macro tsrc+)
        (parse-definition dispatching (match-token-source match) context
                          nil)
      (if (error-display-p cmd-tok-or-error)
          (list cmd-tok-or-error tsrc+)
          (let ((octx (ensure-opaque-context (token-context dispatching))))
            (if octx
                (bind/init ((table (command-table octx) (make-table)))
                  (add-command (token-command-key cmd-tok-or-error)
                               macro table)
                  (list t tsrc+))
                (list (error-display* "noContextInput" dispatching)
                      tsrc+))))))

  (defbuiltin lcdef (match dispatching context)
    "Same as DEF, except that the macro is stored in the localized command
     table for the current locale language (cf. SETLANG)."
    (destructuring-bind (cmd-tok-or-error macro tsrc+)
        (parse-definition dispatching (match-token-source match) context
                          nil)
      (if (error-display-p cmd-tok-or-error)
          (list cmd-tok-or-error tsrc+)
          (let ((octx (ensure-opaque-context
                        (token-context dispatching))))
            (if octx
                (bind/init ((locale-table
                             (locale-table octx)
                             (make-locale-table (context-locale octx))))
                  (let ((command-table (lookup "" locale-table)))
                    (if command-table
                        (progn
                          (add-command
                            (token-command-key cmd-tok-or-error)
                            macro command-table)
                          (list t tsrc+))
                        (list (error-display* "noLocale" dispatching)
                              tsrc+))))
                (list (error-display* "noContextInput" dispatching)
                      tsrc+))))))

  (defbuiltin alias (match dispatching context)
    "Consume one command or active character token, followed by a group or
     singleton token.  Use the first token to obtain a command key.
     Completely expand the contents of the group (or just the singleton
     token) in the local context.  Store the resulting token sequence in the
     alias table of the native context of the dispatching token (or its
     nearest opaque ancestor) under the given command key.  Expand to
     nothing."
    (destructuring-bind (cmd-tok-or-error expn tsrc+)
        (parse-definition dispatching (match-token-source match) context t)
      (if (error-display-p cmd-tok-or-error)
          (list cmd-tok-or-error tsrc+)
          (let ((octx (ensure-opaque-context (token-context dispatching)))
                (expn-tsrc (expansion-to-token-source expn nil nil)))
            (if octx
                (bind/init ((alias-table (alias-table octx) (make-table)))
                  (destructuring-bind (expn* expn-errors)
                      (get-group-tokens expn-tsrc context)
                    (if expn-errors
                        (list (error-display*
                                :add-to expn*
                                :append-message "errorsInAlias"
                                :prepend-input dispatching cmd-tok-or-error)
                              tsrc+)
                        (progn
                          (remember (token-command-key cmd-tok-or-error)
                                    alias-table (ensure-vector expn*))
                          (list t tsrc+)))))
                (list (error-display* "noContextInput" dispatching)
                      tsrc+))))))

  (defun parse-ccat-spec (token-source context)
    "Helper function for the builtins SETCAT and CHR.  Parse input from
     TOKEN-SOURCE for a character category specification.  Return a list
     with three values: the category mask, a flag which is true iff the
     input specifies a base category (as opposed to just toggling the active
     and constituent flags), and a token source pointing after the
     specification."
    (let ((cat 0) (set-base-p nil))
      (macrolet ((set-cat (&rest args)
                   (or (ignore-errors
                         (destructuring-bind (cmd value) args
                           `(if-match-bind ((:command ,(symbol-to-js-string
                                                         cmd)))
                                token-source context
                              (progn (setf cat ,value set-base-p t)
                                     t))))
                       (ignore-errors
                         (destructuring-bind (op cmd compose value) args
                           `(if-match-bind ((:chars ,(string op)
                                             :command ,(symbol-to-js-string
                                                         cmd)))
                                token-source context
                              (progn (setf cat (,compose cat ,value))
                                     t)))))))
        (loop
          while (or (set-cat invalid *ccat-invalid*)
                    (set-cat whitespace *ccat-whitespace*)
                    (set-cat newline *ccat-newline*)
                    (set-cat escape *ccat-escape*)
                    (set-cat param *ccat-param*)
                    (set-cat lbrace *ccat-lbrace*)
                    (set-cat rbrace *ccat-rbrace*)
                    (set-cat letter *ccat-letter*)
                    (set-cat number *ccat-number*)
                    (set-cat other *ccat-other*)
                    (set-cat active *ccat-active*)
                    (set-cat + active logior *ccat-active*)
                    (set-cat - active logandc2 *ccat-active*)
                    (set-cat constituent *ccat-constituent*)
                    (set-cat + constituent logior *ccat-constituent*)
                    (set-cat - constituent logandc2 *ccat-constituent*)))
        (list (if (eq cat 0) *ccat-invalid* cat) set-base-p
              token-source))))

  (defbuiltin setcat (match dispatching context)
    "Consume some tokens that constitute a character category specification,
     and one untokenized character.  Replace the category of the character
     in the category table of the native context of the dispatching
     token (or its nearest opaque ancestor) by the specified category.
     Expand to nothing."
    (destructuring-bind (cat set-base-p tsrc+)
        (parse-ccat-spec (match-token-source match) context)
      (if-match-bind ((c char)) tsrc+ context
        (let ((octx (ensure-opaque-context context)))
          (if octx
              (let ((ctab (category-table octx)))
                (unless set-base-p
                  (setf cat (logior (ccat-base (char-cat c ctab)) cat)))
                (setf (category-table octx) (char-cat c ctab cat))
                (list t tsrc+))
              (list (error-display* "noContextInput" dispatching) tsrc+)))
        (list (error-display* "eotBeforeExpected" dispatching) tsrc+))))

  (defbuiltin chr (match dispatching context)
    "Consume some tokens that constitute a character category specification,
     and one untokenized character.  Expand to one character token with the
     given character value and specified category."
    (destructuring-bind (cat set-base-p tsrc+)
        (parse-ccat-spec (match-token-source match) context)
      (if-match-bind ((c char)) tsrc+ context
        (let ((octx (ensure-opaque-context context)))
          (if octx
              (let ((ctab (if octx (category-table octx)
                              *category-table*))
                    (pos (char-source-offset tsrc+)))
                (unless set-base-p
                  (setf cat (logior (ccat-base (char-cat c ctab)) cat)))
                (list (vector
                        (guarded-make-char-token
                          :start (1- pos) :end pos :context context
                          :chr c :category cat))
                      tsrc+))
              (list (error-display* "noContextInput" dispatching) tsrc+)))
        (list (error-display* "eotBeforeExpected" dispatching) tsrc+))))

)


;;; Local Variables: ***
;;; mode:lisp ***
;;; local-font-lock-keywords:(("(\\(def\\(?:\\(ps\\(?:macro\\|fun\\)\\|\\(?:un\\|macro\\)\\+ps\\|builtin\\)\\|\\(guard\\|enum\\)\\|\\(struct-guarded\\)\\)\\)[ \t\n]+(?\\([^()]*?\\)[ \t\n]" (1 font-lock-keyword-face) (5 (cond ((match-beginning 2) font-lock-function-name-face) ((match-beginning 3) font-lock-variable-name-face) ((match-beginning 4) font-lock-type-face)))) ("(\\(ambi-ps\\)\\_>" (1 font-lock-preprocessor-face)) "\\_<if-match-bind\\_>") ***
;;; End: ***
