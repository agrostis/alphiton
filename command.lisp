(in-package #:mex)

;;; Commands

(ambi-ps ()

  (defvar *command-table* (make-table)
    "The default initial command table, mapping strings (command keys) to
     COMMAND objects.")

  (defstruct (command)
    "A command which may be expanded if the tokens following its dispatching
     token match PATTERN (which is a vector consisting of parameter tokens
     and literal pattern delimiters)."
    pattern)

  (defstruct (builtin (:include command))
    "A builtin command, usually with an empty pattern, and whose expansion
     is dynamically calculated by the function HANDLER."
    handler)

  (defstruct (macro (:include command))
    "A command defined in Mex source with a sequence-of-tokens EXPANSION."
    expansion)

  (defstruct (pattern-delimiter (:conc-name delimiter-))
    "A sequence of literal TOKENS separating parameters in a command
     pattern."
    tokens)

  (defun pattern-part-equal (a b)
    "Return true iff A and B are equal tokens, or if they are pattern
     delimiters consisting of equal tokens."
    (or (and (token-p a) (token-p b) (token-equal a b))
        (and (pattern-delimiter-p a) (pattern-delimiter-p b)
             (vector-equal (delimiter-tokens a) (delimiter-tokens b)
                           #'token-equal))))

  (defun pattern-equal (a b)
    "Return true iff A and B are patterns consisting of the same parameter
     tokens and pattern delimiters."
    (vector-equal a b #'pattern-part-equal))

  (defenum adjoin-force ()
    *adjoin-weak* *adjoin-strong* *adjoin-supersede*)

  (defun adjoin-command (command homonyms force)
    "If no element of HOMONYMS (a vector of commands) has the same pattern
     as COMMAND, return a vector containing COMMAND and every element of
     HOMONYMS.  If an element of HOMONYMS has the same pattern as COMMAND,
     either discard COMMAND and return HOMONYMS verbatim, or return a copy
     of HOMONYMS with that element replaced by COMMAND.  The last choice
     depends on the types of the equipattern element and of COMMAND, and on
     the value of FORCE."
    (loop for i :from 0 :below (length homonyms)
          for homonym := (aref homonyms i)
          if (pattern-equal (command-pattern command)
                            (command-pattern homonym))
            return
              (if (or (eq force *adjoin-supersede*)
                      (and (or (not (builtin-p homonym))
                               (builtin-p command))
                           (eq force *adjoin-strong*)))
                  (spliced homonyms i 1 command)
                  homonyms)
          finally (return (spliced homonyms 0 0 command))))

  (defun add-command (name command &optional (table *command-table*) forcep)
    "Add COMMAND to TABLE under the key NAME (a table may have multiple
     commands with a given name, distinguished by their patterns).  If TABLE
     has another command with the same name and pattern, and FORCEP is
     false, COMMAND is discarded; but if FORCEP is true, COMMAND replaces
     the prior definition (except that a builtin is never replaced by a
     non-builtin)."
    (let* ((homonyms (lookup name table #()))
           (adjoined (adjoin-command
                       command homonyms
                       (if forcep *adjoin-strong* *adjoin-weak*))))
      (unless (eq homonyms adjoined)
        (remember name table adjoined))))

  (defun add-const-macro (name expansion
                          &optional (table *command-table*) forcep)
    "Add to TABLE, under the key NAME, a constant macro, i. e. a macro with
     an empty pattern and the given EXPANSION."
    (let ((cm (make-macro :pattern #() :expansion expansion)))
      (add-command name cm table forcep)))

  (defun lookup-commands (name context)
    "Return a vector of commands recorded under the given NAME in the
     command tables of CONTEXT and of its ancestor contexts.  If no such
     command exists in any context, return null.  A command in a descendant
     context which has the same pattern as a homonymous command in an
     ancestor context takes precedence."
    (let ((all-commands #()))
      (labels ((merge-commands (commands)
                 (loop for cmd :across commands
                       do (setf all-commands
                                  (adjoin-command
                                    cmd commands *adjoin-supersede*))))
               (merge-ctx (context)
                 (when (command-table context)
                   (merge-commands
                     (lookup name (command-table context) #())))
                 (when (locale-table context)
                   (let ((current-locale-command-table
                           (lookup "" (locale-table context)))
                         (default-locale-command-table
                           (lookup *default-locale*
                                   (locale-table context))))
                     (merge-commands
                       (or (and current-locale-command-table
                                (lookup name current-locale-command-table))
                           (and default-locale-command-table
                                (lookup name default-locale-command-table))
                           #())))))
               (browse-ctx (context)
                 (when (parent-context context)
                   (browse-ctx (parent-context context)))
                 (merge-ctx context)))
        (browse-ctx context)
        (and (> (length all-commands) 0) all-commands))))

  (defun lookup-const-command (name context)
    "Return a constant command (i. e. one with an empty pattern) recorded
     under NAME in CONTEXT, or in its nearest ancestor context.  If no such
     command exists in any context, return null."
    (loop
      for ctx := context :then (parent-context ctx)
      while ctx
      for cmd := (let* ((tab (command-table ctx))
                        (locales (locale-table ctx))
                        (ltab (and locales (lookup "" locales)))
                        (dltab (and locales
                                    (lookup *default-locale* locales)))
                        (cmds (or (and tab (lookup name tab))
                                  (and ltab (lookup name ltab))
                                  (and dltab (lookup name dltab))
                                  #())))
                   (loop for cmd :across cmds
                         if (= (length (command-pattern cmd)) 0)
                           return cmd))
      if cmd return cmd))

  (defstruct (command-match (:conc-name))
    "State of the parser after the pattern of MATCHED-COMMAND has been
     completely matched against the input.  MATCH-CONTEXT's command table
     binds names of the pattern's parameter tokens to sequences of tokens
     and / or groups in the use of the command.  MATCH-TOKEN-SOURCE is a
     token source pointing after the matched input.  MATCH-LENGTH is the
     length, in tokens, of the matched input, and MATCHED-TOKEN-COUNT is the
     number of literal tokens in token delimiters."
    matched-command match-context match-token-source
    match-length matched-token-count)

  (defstruct (partial-match (:conc-name))
    "State of the parser after some part of a command pattern (a delimiter
     or a parameter) has been matched against the input.  DELTA-MATCH-LENGTH
     is the length, in tokens, of the input sequence that matches the
     pattern part in question.  DELTA-TOKEN-COUNT is 0 for parameters and
     the token count for delimiters.  PM-TOKEN-SOURCE a token source
     pointing after the matched input.  PARAM-EXPANSION is null for
     delimiters; for parameters, it is a vector of input tokens and/or
     groups matching the parameter in question."
    delta-match-length delta-token-count pm-token-source param-expansion)

  (declaim
    (ftype function
      match-param-delimited match-param-single match-pattern-delimiter))

  (defun match-command (command token-source context)
    "Match the pattern of COMMAND against the input in TOKEN-SOURCE.  Return
     either a COMMAND-MATCH object, or null, if the input does not match the
     pattern."
    (let* ((pattern (command-pattern command))
           (tsrc0-ctx (expansion-context token-source))
           (ctab (category-table context)))
      (loop
        with i := -1 while (< (incf i) (length pattern))
        for part := (aref pattern i)
        with match-length := 0
        with matched-token-count := 0
        with bindings := (make-table)
        do (let ((pm (cond
                       ((param-token-p part)
                        (let ((delim (aref* pattern (1+ i))))
                          (if (and delim (not (param-token-p delim)))
                              (progn
                                (incf i)
                                (match-param-delimited
                                  delim token-source context))
                              (match-param-single token-source context))))
                       ((pattern-delimiter-p part)
                        (match-pattern-delimiter part token-source context))
                       (t nil))))
             (if pm
                 (progn
                   (setf token-source (pm-token-source pm))
                   (incf match-length (delta-match-length pm))
                   (incf matched-token-count (delta-token-count pm))
                   (when (param-expansion pm)
                     (add-const-macro (token-command-key part)
                                      (param-expansion pm)
                                      bindings)))
                 (return nil)))
        finally
          (return
            (make-command-match
              :matched-command command
              :match-context (guarded-make-context
                               :category-table ctab
                               :command-table bindings
                               :parent-context tsrc0-ctx)
              :match-token-source token-source
              :match-length match-length
              :matched-token-count matched-token-count)))))

  (defun match-pattern-delimiter (delimiter token-source context)
    "Match DELIMITER against the input in TOKEN-SOURCE.  Return either a
     PARTIAL-MATCH object, or null, if the input does not match DELIMITER
     (i. e., does not consist of the same tokens)."
    (loop
      with delim-tokens := (delimiter-tokens delimiter)
      with length := (length delim-tokens)
      for i :from 0 :below length
      for tok := (next-token token-source context)
      if (not (token-equal tok (elt delim-tokens i)))
        return nil
      finally (return
                (make-partial-match
                  :delta-match-length length
                  :delta-token-count length
                  :pm-token-source token-source))))

  (defun match-param-delimited (delimiter token-source context)
    "Match DELIMITER against the input in TOKEN-SOURCE which follows some
     (possibly empty) prefix, taken to match a parameter that precedes
     DELIMITER in a command pattern.  The prefix should be entirely
     contained in one paragraph and in one group (not counting subgroups).
     Return either a PARTIAL-MATCH object, or null, if the input does not
     match DELIMITER after a prefix."
    (loop
      for delimiter-pm := (match-pattern-delimiter
                            delimiter token-source context)
      for input-elt := (and (not delimiter-pm)
                            (value1 (next-group token-source context)))
      if delimiter-pm
        do (incf (delta-match-length delimiter-pm) match-length)
           (setf (param-expansion delimiter-pm) (ensure-vector expn))
        and return delimiter-pm
      else if (or (funcall *group-end-p* input-elt)
                  (par-break-p input-elt)
                  (eot-p input-elt))
        return nil
      else
        collect input-elt :into expn
        sum (if (group-p input-elt) (group-token-count input-elt) 1)
          :into match-length))

  (defun match-param-single (token-source context)
    "Parse TOKEN-SOURCE for one group or singleton token which is taken to
     match a parameter in a command pattern.  Return a PARTIAL-MATCH
     object."
    (let ((input-elt (value1 (next-group token-source context))))
      (and (not (or (funcall *group-end-p* input-elt) (eot-p input-elt)))
           (if (group-p input-elt)
               (make-partial-match
                 :delta-match-length (group-token-count input-elt)
                 :delta-token-count 0
                 :param-expansion (group-contents input-elt)
                 :pm-token-source token-source)
               (make-partial-match
                 :delta-match-length 1
                 :delta-token-count 0
                 :param-expansion (vector input-elt)
                 :pm-token-source token-source)))))

  (defun match-command-best (commands token-source context)
    "Match every command in the vector COMMANDS against the input in
     TOKEN-SOURCE and return a COMMAND-MATCH object.  If more than one
     command matches the input, order the matches by MATCHED-TOKEN-COUNT,
     further by MATCH-LENGTH, further by command type (builtins precede
     macros), and select the best match.  If no command matches the input,
     return null."
    (loop for cmd :across commands
          with best-match := nil
          do (let ((match (match-command cmd token-source context)))
               (when match
                 (let ((mc (matched-command match))
                       (ml (match-length match))
                       (mtc (matched-token-count match)))
                   (when (or (not best-match)
                             (let ((bmc (matched-command best-match))
                                   (bml (match-length best-match))
                                   (bmtc (matched-token-count best-match)))
                               (or (> mtc bmtc)
                                   (and (= mtc bmtc) (> ml bml))
                                   (and (builtin-p mc) (macro-p bmc)))))
                     (setf best-match match)))))
          finally (return best-match)))

  (defun parser-expansion-state (token-source expansion)
    "Return a parser state after a command possibly has been matched and
     expanded.  The accumulator contains the expansion, and the value is
     true iff the parser state indeed resulted from an expansion (as opposed
     to just peeking a non-expandable token)."
    (if (eq expansion t)
        (make-parser-state :token-source-state token-source :parser-value t)
        (make-parser-state :token-source-state token-source
                           :accumulator expansion
                           :parser-value (and expansion t))))

  (defgeneric command-expansion (command match dispatching context)
    (:documentation "Get the sequence of tokens and/or groups that COMMAND
      expands to.  Additionally return the token source pointing after the
      matched input (and, for builtins, after the input consumed by the
      handler).")
    (:method ((command builtin) match dispatching context)
      (funcall (builtin-handler command) match dispatching context))
    (:method ((command macro) match dispatching context)
      (parser-expansion-state (match-token-source match)
                              (macro-expansion command)))
    (:method (command match dispatching context)
      (parser-expansion-state (match-token-source match) nil)))

  (defun expansion-to-token-source (expansion parent-source context)
    "Convert a command expansion (vector of tokens and/or groups) into a
     token source such that NEXT-TOKEN and SOURCE-AFTER-NEXT-TOKEN
     would traverse preserving the order."
    (let ((tokens (make-stack)))
      (labels ((push-tokens (input)
                 (loop for sub :across input
                       if (group-p sub)
                         do (stack-push (group-lbrace sub) tokens)
                            (push-tokens (group-contents sub))
                            (stack-push (group-rbrace sub) tokens)
                       else
                         do (stack-push sub tokens))))
        (if (vectorp expansion)
            (push-tokens expansion)
            (stack-push expansion tokens))
        (make-token-source :cached-tokens (ensure-vector tokens)
                           :cached-token-offset 0
                           :parent-source parent-source
                           :expansion-context context))))

  (defun expand-commands (commands dispatching token-source context)
    "Match COMMANDS (obtained by resolving the token DISPATCHING) against
     the input in TOKEN-SOURCE, as by MATCH-COMMAND-BEST, convert the
     expansion to a token source, and return it.  If a matching error
     occurs, an error display is substituted for the expansion."
    (let ((match
           (and (vectorp commands)
                (match-command-best commands token-source context))))
      (if match
          (parser-state-bind (:accumulator expansion :token-source tsrc+)
              (command-expansion (matched-command match)
                                 match dispatching context)
            (if expansion
                (expansion-to-token-source expn tsrc+ (match-context match))
                tsrc+))
          (let ((input (if (stringp dispatching)
                           (make-command-token
                             :escape-chr (char-at dispatching 0)
                             :name (substring dispatching 1)
                             :context context)
                           dispatching)))
            (expansion-to-token-source
              (if commands
                  (error-display* "noMatchingDef" input)
                  (error-display* "undefined" input))
              token-source nil)))))

  (defun expand-params-and-noexpands (input token-source context)
    "Expand any parameters at the beginning of INPUT, and remove protective
     \noexpand.  If there was an expansion, return a parser state with value
     NIL and the token source resulting from the expansion; otherwise,
     return a parser state with the non-expandable token as value and the
     token source after it."
    (cond
      ((param-token-p input)
       (parser-state-bind (tsrc+ expd)
           (mex-dispatch input token-source context)
         (if expd
             (parser-state tsrc+ nil)
             (parser-state token-source input))))
      ((special-command-p input "noexpand")
       (let ((tok (next-token/shift token-source context nil)))
         (parser-state token-source tok)))
      (t (parser-state token-source input))))

  (defvar *unexpandable* '()
    "List of command and active tokens that should not expand,
     notwithstanding the general rule.")

  (defvar *unexpandable-params* '()
    "List of parameter tokens that should not expand, notwithstanding
    the general rule.")

  (defun mex-dispatch (input token-source context)
    "If INPUT is expandable (i. e., is a command, active character, or
     parameter token), look up its command bindings in the appropriate
     context(s), match the commands against the input in TOKEN-SOURCE, and
     expand the best-matching command.  Return a parser state with a boolean
     value which is true iff INPUT is expandable, and the token source
     resulting from consuming INPUT and inserting expansion, where
     appropriate."
    (let* ((cmd-key
            (token-command-key input))
           (commands
            (cond
              ((and (dispatching-token-p input)
                    (not (or (eq *unexpandable* t)
                             (member input *unexpandable*
                                     :test #'token-equal))))
               (lookup-commands cmd-key (token-context input)))
              ((and (param-token-p input)
                    (not (member input *unexpandable-params*
                                 :test #'token-equal)))
               (let ((ectx (expansion-context token-source)))
                 (or (and ectx
                          (let ((cmd (lookup-const-command cmd-key ectx)))
                            (and cmd (vector cmd))))
                     (lookup-commands cmd-key context))))
              (t t))))
      (if (eq commands t)
          (parser-state token-source nil)
          (parser-state (expand-commands commands input token-source context)
                        t))))

  (defun simulate-command-with-input (name token-source context
                                      value-if-undefined)
    "Look up commands bound to NAME in CONTEXT, match them against input
     from TOKEN-SOURCE, and expand the best-matching command.  Return the
     token source resulting from expansion."
    (let* ((key (command-key name))
           (commands (lookup-commands key context)))
      (if (and (not commands) (not (eq value-if-undefined t)))
          value-if-undefined
          (let* ((context* (make-opaque-context
                             :category-table (category-table context)
                             :parent-context context))
                 (expn (expand-commands commands key token-source
                                        context*)))
            (setf (parent-source expn) nil)
            expn))))

  (defun simulate-command-with-tokens (name tokens parent-source context
                                       value-if-undefined)
    "Same as SIMULATE-COMMAND-WITH-INPUT, but match commands against the
     input in the vector TOKENS."
    (loop for tok :across tokens
          do (setf (token-context tok) context))
    (let ((tsrc (expansion-to-token-source tokens parent-source nil)))
      (simulate-command-with-input name tsrc context value-if-undefined)))

  (defmacro simulate-command (name input context
                              &optional parent-source value-if-undefined)
    "Simulate the expansion of a command named NAME, as by
     SIMULATE-COMMAND-WITH-INPUT."
    (if input
        `(simulate-command-with-tokens
           ,name ,(apply #'tokens** input) ,parent-source ,context
           ,value-if-undefined)
        `(simulate-command-with-input
           ,name ,parent-source ,context ,value-if-undefined)))

  (defmacro next-token/expand (token-source context)
    "Same as NEXT-TOKEN/SHIFT, but treat \noexpand specially, and expand
     parameters."
    (with-ps-gensyms (tok0 pstate tsrc)
      `(loop with ,tsrc := ,token-source
             for ,tok0 := (next-token/shift ,tsrc ,context)
             for ,pstate := (expand-params-and-noexpands
                              ,tok0 ,tsrc ,context)
             if (value1 ,pstate)
               do (setf ,token-source (token-source-state ,pstate))
               and return (value1 ,pstate)
             else
               do (setf ,tsrc (token-source-state ,pstate)))))

  (defmacro next-group/expand (token-source context)
    "Same as NEXT-GROUP/SHIFT, but treat \noexpand specially, and expand
     parameters."
    (with-ps-gensyms (grp0 pstate tsrc)
      `(loop with ,tsrc := ,token-source
             for ,grp0 := (next-group/shift ,tsrc ,context)
             for ,pstate := (expand-params-and-noexpands
                              ,grp0 ,tsrc ,context)
             if (value1 ,pstate)
               do (setf ,token-source (token-source-state ,pstate))
               and return (value1 ,pstate)
             else
               do (setf ,tsrc (token-source-state ,pstate)))))

)


;;; Builtins

(defmacro+ps defbuiltin (name
                         (match-var dispatching-var context-var
                          &key (patterns nil)
                               (table '*command-table*)
                               (token-source nil))
                         &body body)
  "Like DEFUN, but defines a command expansion handler bound to (COMMAND-KEY
   NAME) in TABLE.  Any handler must accept three arguments: a COMMAND-MATCH
   object, the token which triggered the command, and the local context.  It
   must return a parser state with the value being the expansion, which may
   take the form of one token or error display, or of a vector of tokens
   and/or groups, or of the boolean true meaning a trivial (that is, empty)
   expansion."
  (let ((docstring (if (stringp (car body)) (pop body)))
        (handler-var (gensym "HNDLR"))
        (patterns (if patterns
                      (mapcar (lambda (pattern) `(tokens ,@pattern))
                              patterns)
                      (list (vector))))
        (cmd (command-key name)))
    `(let ((,handler-var
            (lambda (,match-var ,dispatching-var ,context-var)
              ,@(if docstring `(,docstring))
              (block ,name
                ,@(if token-source
                      `((let ((,token-source
                                (match-token-source ,match-var)))
                          ,@body))
                      body)))))
       ,@(loop for pattern :in patterns
               collect `(add-command ,cmd
                          (make-builtin
                            :pattern ,pattern
                            :handler ,handler-var)
                          ,table t)))))

(defmacro+ps bind-match-params ((&rest bindings) match &body body)
  "Evaluate BODY in an environment where variables from BINDINGS are bound
   to expansions of eponymous parameters in MATCH."
  (with-var-value (match)
    `(let ,(loop for binding :in bindings
                 for (param . default)
                   := (or (ignore-errors
                            (destructuring-bind (p d) binding (cons p d)))
                          (cons binding nil))
                 collect `(,param
                           (let ((cmd (lookup-const-command
                                        ,(command-key param)
                                        (match-context ,match))))
                             (if (macro-p cmd)
                                 (macro-expansion cmd)
                                 ,default))))
       ,@body)))
                         

(defmacro+ps if-match-bind ((&rest pattern) token-source context
                            seq &optional alt)
  "If the input at the beginning of TOKEN-SOURCE matches the sequence of
   characters, tokens, and/or groups represented by PATTERN, run SEQ in an
   environment where variables from PATTERN are bound to characters, tokens,
   or groups parsed out from the input.  If the input doesn't match, and ALT
   is provided, run it instead.
     An element of PATTERN is either a list of inputs acceptable by
   TOKENS* (which matches an equal sequence of tokens), or a spec of the
   form (<VAR> <TYPE> [<PRED>]), where <VAR> is a symbol (variable name),
   <TYPE> is one of the literal symbols CHAR, TOKEN, GROUP, and <PRED> is an
   optional specifier for a predicate function, quasi defaulting
   to (CONSTANTLY T).  A spec of this form matches/binds a character, token,
   or group of input that satisfies <PRED>."
  `(let (,@(loop for part :in pattern
                 if (and (consp part)
                         (symbolp (car part))
                         (not (keywordp (car part))))
                   collect (car part)))
     (if (and
          ,@(mapcar
             (lambda (part)
               (or (ignore-errors
                     (destructuring-bind (var type &optional pred) part
                       (and (symbolp var)
                            (let* ((extract-op
                                    (case type
                                      ((char) 'next-char/shift)
                                      ((token) 'next-token/shift)
                                      ((token*) 'next-token/expand)
                                      ((group) 'next-group/shift)
                                      ((group*) 'next-group/expand)
                                      (t nil)))
                                   (extract
                                    `(,extract-op ,token-source ,context)))
                              (and extract-op
                                   `(progn
                                      (setf ,var ,extract)
                                      ,@(and pred
                                             `((funcall ,pred ,var)))))))))
                   (ignore-errors
                     (let* ((tokens (apply #'tokens** part))
                            (pm (gensym "PM")))
                       (and tokens
                            `(let ((,pm (match-pattern-delimiter
                                          (make-pattern-delimiter
                                            :tokens ,tokens)
                                          ,token-source ,context)))
                               (and ,pm
                                    (setf ,token-source
                                            (pm-token-source ,pm)))))))
                   (error "Invalid MATCH-BIND pattern part: ~S" part)))
             pattern))
          ,seq
          ,alt)))


;;; Local Variables: ***
;;; mode:lisp ***
;;; local-font-lock-keywords:(("(\\(def\\(?:\\(ps\\(?:macro\\|fun\\)\\|\\(?:un\\|macro\\)\\+ps\\|builtin\\)\\|\\(guard\\|enum\\)\\|\\(struct-guarded\\)\\)\\)[ \t\n]+(?\\([^()]*?\\)[ \t\n]" (1 font-lock-keyword-face) (5 (cond ((match-beginning 2) font-lock-function-name-face) ((match-beginning 3) font-lock-variable-name-face) ((match-beginning 4) font-lock-type-face)))) ("(\\(ambi-ps\\)\\_>" (1 font-lock-preprocessor-face)) "\\_<if-match-bind\\_>") ***
;;; End: ***
