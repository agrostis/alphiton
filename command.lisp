(in-package #:mex)

;;; Commands

(ambi-ps ()

  (defvar *command-table* (make-table)
    "The default initial command table, mapping strings (command keys) to
     COMMAND objects.")

  (defstruct (command)
    "A command which may be expanded if the tokens following its dispatching
     token match PATTERN (which is either a vector consisting of parameter
     tokens and literal pattern delimiters, or a function to perform dynamic
     matching)."
    pattern)

  (defstruct (builtin (:include command))
    "A builtin command, whose pattern is normally either empty or a function,
     and whose expansion is dynamically calculated by the function HANDLER."
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
    (or (eq a b)
        (and (vectorp a) (vectorp b)
             (vector-equal a b #'pattern-part-equal))))

  (defun pattern-tokens (pattern)
    "Return the tokens which make up PATTERN as a flat sequence."
    (loop with toks := (stack)
          for part :in pattern
          if (token-p part)
            do (stack-push part toks)
          else if (pattern-delimiter-p part)
            do (loop for tok :across (delimiter-tokens part)
                     do (stack-push tok toks))
          finally (return (ensure-vector toks))))

  (defenum adjoin-force ()
    *adjoin-weak* *adjoin-strong* *adjoin-supersede* *adjoin-append*)

  (defun adjoin-command (command homonyms force)
    "If no element of HOMONYMS (a vector of commands) has the same pattern
     as COMMAND, return a vector containing COMMAND and every element of
     HOMONYMS.  If an element of HOMONYMS has the same pattern as COMMAND,
     either discard COMMAND and return HOMONYMS verbatim, or return a copy
     of HOMONYMS with that element replaced by COMMAND, or change the
     element's expansion by appending it to the expansion of COMMAND.
     The choice depends on the types of the equipattern element and of
     COMMAND, and on the value of FORCE:
       * If FORCE is *ADJOIN-WEAK*, COMMAND is discarded.
       * If FORCE is *ADJOIN-STRONG*, the equipattern element is replaced,
         except if it is a builtin, and COMMAND a macro.
       * If FORCE is *ADJOIN-SUPERSEDE*, the equipattern element is
         replaced unconditionally.
       * If FORCE is *ADJOIN-APPEND* and both COMMAND and the equipattern
         element are macros, the expansion of COMMAND is appended.
         Otherwise, *ADJOIN-APPEND* works like *ADJOIN-STRONG*."
    (loop for i :from 0 :below (length homonyms)
          for homonym := (aref homonyms i)
          if (pattern-equal (command-pattern command)
                            (command-pattern homonym))
            do (when (and (eq force *adjoin-append*)
                          (macro-p command) (macro-p homonym))
                 (setf command
                         (make-macro
                           :pattern (command-pattern command)
                           :expansion (vector-add
                                        (macro-expansion homonym)
                                        (macro-expansion command)))))
            and return
              (if (or (eq force *adjoin-supersede*)
                      (and (or (eq force *adjoin-strong*)
                               (eq force *adjoin-append*))
                           (or (not (builtin-p homonym))
                               (builtin-p command))))
                  (spliced homonyms i 1 command)
                  homonyms)
          finally (return (spliced homonyms 0 0 command))))

  (defun add-command (name command
                      &optional (table *command-table*)
                                (force *adjoin-weak*))
    "Add COMMAND to TABLE under the key NAME (a table may have multiple
     commands with a given name, distinguished by their patterns).  If TABLE
     has another command with the same name and pattern, the conflict is
     resolved according to the types of the two commands and the value of
     FORCE (see ADJOIN-COMMAND)."
    (let* ((homonyms (lookup name table #()))
           (adjoined (adjoin-command command homonyms force)))
      (unless (eq homonyms adjoined)
        (remember name table adjoined))))

  (defun add-const-macro (name expansion
                          &optional (table *command-table*)
                                    (force *adjoin-weak*))
    "Add to TABLE, under the key NAME, a constant macro, i. e. a macro with
     an empty pattern and the given EXPANSION."
    (let ((cm (make-macro :pattern #() :expansion expansion)))
      (add-command name cm table force)))

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
                                    cmd all-commands *adjoin-supersede*))))
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

  (defstruct (command-match (:conc-name) (:include parser-state))
    "State of the parser after the pattern of a command has been completely
     matched against the input.  The value is the matched command, and the
     token source points after the matched input.  MATCH-CONTEXT's command
     table binds names of the pattern's parameter tokens to sequences of
     tokens and / or groups in the matched input.  MATCH-LENGTH is the
     length, in tokens, of the matched input, and MATCHED-TOKEN-COUNT is the
     number of literal tokens in token delimiters."
    match-context dispatching-token match-length matched-token-count)

  (defstruct (partial-match (:conc-name) (:include parser-state))
    "State of the parser after some part of a command pattern (a delimiter
     or a parameter) has been matched against the input.  The accumulator is
     null for delimiters; for parameters, it holds input tokens and/or
     groups matching the parameter in question.  The token source points
     after the matched input.  DELTA-MATCH-LENGTH is the length, in tokens,
     of the input sequence that matches the pattern part in question.
     DELTA-TOKEN-COUNT is 0 for parameters and the token count for
     delimiters."
    delta-match-length delta-token-count)

  (declaim
    (ftype function
      match-param-delimited match-param-single match-pattern-delimiter))

  (defun match-command (command dispatching token-source context)
    "Match the pattern of COMMAND against the input in TOKEN-SOURCE.  Return
     either a COMMAND-MATCH object, or null, if the input does not match the
     pattern."
    (let ((pattern (command-pattern command))
          (tsrc0-ctx (expansion-context token-source))
          (ctab (category-table context)))
      (if (functionp pattern)
          (let ((match (make-command-match
                         :parser-value command
                         :dispatching-token dispatching
                         :token-source-state token-source
                         :match-context tsrc0-ctx
                         :match-length 0
                         :matched-token-count 0)))
            (funcall pattern match context))
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
                                  (match-param-single
                                    token-source context))))
                           ((pattern-delimiter-p part)
                            (match-pattern-delimiter
                              part token-source context))
                           (t nil))))
                 (if pm
                     (progn
                       (setf token-source (token-source-state pm))
                       (incf match-length (delta-match-length pm))
                       (incf matched-token-count (delta-token-count pm))
                       (when (accumulator pm)
                         (add-const-macro (token-command-key part)
                                          (accumulator pm)
                                          bindings
                                          *adjoin-append*)))
                     (return nil)))
            finally (return
                      (make-command-match
                        :parser-value command
                        :dispatching-token dispatching
                        :token-source-state token-source
                        :match-context (spawn-context tsrc0-ctx
                                           (guarded context)
                                         :category-table ctab
                                         :command-table bindings)
                        :match-length match-length
                        :matched-token-count matched-token-count))))))

  (defun match-pattern-delimiter (delimiter token-source context)
    "Match DELIMITER against the input in TOKEN-SOURCE.  Return either a
     PARTIAL-MATCH object, or null, if the input does not match DELIMITER
     (i. e., does not consist of the same tokens)."
    (loop
      with delim-tokens := (delimiter-tokens delimiter)
      with length := (length delim-tokens)
      for i :from 0 :below length
      for tok := (next-token/shift token-source context)
      if (not (token-equal tok (elt delim-tokens i)))
        return nil
      finally (return
                (make-partial-match
                  :delta-match-length length
                  :delta-token-count length
                  :token-source-state token-source))))

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
      for input-elt-ps := nil
      for input-elt := nil
      if delimiter-pm
        do (incf (delta-match-length delimiter-pm) match-length)
           (setf (accumulator delimiter-pm) (ensure-vector expn))
        and return delimiter-pm
      else do (setq input-elt-ps (next-group token-source context))
        and if (parser-error input-elt-ps)
          return nil
        else 
          do (setq input-elt (accumulator input-elt-ps))
      if (or (funcall *group-end-p* input-elt)
             (par-break-p input-elt)
             (eot-p input-elt))
        return nil
      else
        collect input-elt :into expn
        and sum (if (group-p input-elt) (group-token-count input-elt) 1)
          :into match-length
        and do (setf token-source (token-source-state input-elt-ps))))

  (defun match-param-single (token-source context)
    "Parse TOKEN-SOURCE for one group or singleton token which is taken to
     match a parameter in a command pattern.  Return a PARTIAL-MATCH
     object, or null, if the input doesn't match."
    (parser-state-bind (:accumulator input-elt :error input-error
                        :token-source tsrc+)
        (next-group token-source context)
      (and (not (or input-error
                    (funcall *group-end-p* input-elt)
                    (par-break-p input-elt)
                    (eot-p input-elt)))
           (if (group-p input-elt)
               (make-partial-match
                 :delta-match-length (group-token-count input-elt)
                 :delta-token-count 0
                 :accumulator (group-contents input-elt)
                 :token-source-state tsrc+)
               (make-partial-match
                 :delta-match-length 1
                 :delta-token-count 0
                 :accumulator (vector input-elt)
                 :token-source-state tsrc+)))))

  (defun match-command-best (commands dispatching token-source context)
    "Match every command in the vector COMMANDS against the input in
     TOKEN-SOURCE and return a COMMAND-MATCH object.  If more than one
     command matches the input, order the matches by MATCHED-TOKEN-COUNT,
     further by MATCH-LENGTH, further by command type (builtins precede
     macros), and select the best match.  If no command matches the input,
     return null."
    (loop for cmd :across commands
          with best-match := nil
          do (let ((match (match-command
                            cmd dispatching token-source context)))
               (when (and match
                          (or (not best-match)
                              (let ((cmd (parser-value match))
                                    (ml (match-length match))
                                    (mtc (matched-token-count match))
                                    (bcmd (parser-value best-match))
                                    (bml (match-length best-match))
                                    (bmtc (matched-token-count best-match)))
                                (or (> mtc bmtc)
                                    (and (= mtc bmtc) (> ml bml))
                                    (and (builtin-p cmd) (macro-p bcmd))))))
                 (setf best-match match)))
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

  (defgeneric command-expansion (command match context ship)
    (:documentation "Get the sequence of tokens and/or groups that COMMAND
      expands to (if any).  Return a parser state, as produced by
      PARSER-EXPANSION-STATE.")
    (:method ((command builtin) match context ship)
      (funcall (builtin-handler command) match context ship))
    (:method ((command macro) match context ship)
      (declare (ignore context ship))
      (parser-expansion-state (token-source-state match)
                              (macro-expansion command)))
    (:method (no-command match context ship)
      (declare (ignore no-command context ship))
      (parser-expansion-state (token-source-state match)
                              nil)))

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

  (defun mex-expand (commands dispatching token-source context ship)
    "Match COMMANDS (obtained by resolving the token DISPATCHING) against
     the input in TOKEN-SOURCE, as by MATCH-COMMAND-BEST, convert the
     expansion to a token source, and return it.  If a matching error
     occurs, an error display is substituted for the expansion."
    (let* ((dispatching-input
            (if (stringp dispatching)
                (make-command-token
                  :escape-chr (char-at dispatching 0)
                  :name (substring dispatching 1)
                  :context context)
                dispatching))
           (match
            (and (vectorp commands)
                 (match-command-best commands dispatching-input token-source
                                     context))))
      (if match
          (let ((command (parser-value match))
                (mctx (match-context match)))
            (parser-state-bind (:accumulator expansion :error error
                                :token-source tsrc+)
                (command-expansion command match context ship)
              (let ((expansion* (or error expansion)))
                (if expansion*
                    (expansion-to-token-source expansion* tsrc+ mctx)
                    tsrc+))))
          (expansion-to-token-source
            (if dispatching
                (if commands
                    (error-display* "noMatchingDef" dispatching-input)
                    (error-display* "undefined" dispatching-input))
                ;; The following is the case when we do dynamic matching,
                ;; such as in EXPAND-ITERATION-ON-INPUT.
                (let* ((cmd (aref* commands 0))
                       (pattern0 (and cmd (command-pattern cmd)))
                       (pattern (if (vectorp pattern0)
                                    pattern0
                                    (tokens :chars "<Pattern>")))
                       (input (if (char-source token-source)
                                  (tokens :chars "<Character source>")
                                  (cached-tokens token-source))))
                  (make-error-display
                    :message (tokens :command "patternMismatch")
                    :faulty-input (vector-add
                                    (tokens :chars "{")
                                    pattern
                                    (tokens :chars "}{")
                                    input
                                    (tokens :chars "}")))))
            token-source nil))))


  (defvar *unexpandable* '()
    "List of command and active tokens that should not expand,
     notwithstanding the general rule.")

  (defvar *unexpandable-params* '()
    "List of parameter tokens that should not expand, notwithstanding
    the general rule.")

  #| TBD: Make unexpandables context-local (consider \def within \edef). |#

  (defun mex-dispatch (input token-source context ship)
    "If INPUT is an expandable token (i. e., if it is a command, active
     character, or parameter token, and is not excluded per *UNEXPANDABLE*
     or *UNEXPANDABLE-PARAMS*), look up the token's command bindings in the
     appropriate context(s), match the commands against the input in
     TOKEN-SOURCE, and expand the best-matching command.  Return a parser
     state with a boolean value which is true iff INPUT was expandable, and
     the token source resulting from consuming INPUT and inserting
     expansion, where appropriate."
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
          (make-parser-state
            :token-source-state token-source :parser-value nil)
          (let ((expn-tsrc (mex-expand commands input token-source context
                                       ship)))
            (make-parser-state
              :token-source-state expn-tsrc :parser-value t)))))

  (defun get-full-expansion (input token-source context)
    (let* ((ectx (and token-source (expansion-context token-source)))
           (tsrc+ (expansion-to-token-source
                    (if (group-p input) (group-contents input) input)
                    nil ectx)))
      (get-group-tokens tsrc+ context)))
 
  (defun simulate-command-with-input (name token-source context
                                      value-if-undefined)
    "Look up commands bound to NAME in CONTEXT, match them against input
     from TOKEN-SOURCE, and expand the best-matching command.  Return the
     token source resulting from expansion."
    (let* ((key (command-key name))
           (commands (lookup-commands key context)))
      (if (and (not commands) (not (eq value-if-undefined t)))
          value-if-undefined
          (let* ((context* (spawn-context context (opaque-context)))
                 (expn (mex-expand commands key token-source context* nil)))
            (setf (parent-source expn) nil)
            expn))))

  (defun simulate-command-with-tokens (name tokens parent-source context
                                       value-if-undefined)
    "Same as SIMULATE-COMMAND-WITH-INPUT, but match commands against the
     input in the vector TOKENS."
    (loop for tok :across tokens
          do (setf (token-context tok) context))
    (let* ((ectx (and parent-source (expansion-context parent-source)))
           (tsrc (expansion-to-token-source tokens parent-source ectx)))
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
    "Like NEXT-TOKEN/SHIFT, but expand parameters and remove protective
     \\noexpand."
    (with-ps-gensyms (tok0 pstate tsrc)
      `(let ((*unexpandable* t))
         (loop with ,tsrc := ,token-source
               with ,pstate
               for ,tok0 := (next-token/shift ,tsrc ,context)
               if (special-command-p ,tok0 "noexpand")
                 return (prog1 (next-token/shift ,tsrc ,context)
                          (setf ,token-source ,tsrc))
               else
                 do (setf ,pstate (mex-dispatch ,tok0 ,tsrc ,context nil))
                    (setf ,tsrc (token-source-state ,pstate))
               if (not (parser-value ,pstate))
                 return (progn (setf ,token-source ,tsrc)
                               ,tok0)))))

  (defmacro next-group/expand (token-source context)
    "Like NEXT-GROUP/SHIFT, but expand parameters and remove protective
     \\noexpand."
    (with-ps-gensyms (grp0 pstate tsrc)
      `(let ((*unexpandable* t))
         (loop with ,tsrc := ,token-source
               with ,pstate
               for ,grp0 := (next-group/shift ,tsrc ,context t)
               if (group-p ,grp0)
                 return (progn (setf ,token-source ,tsrc)
                               ,grp0)
               else if (special-command-p ,grp0 "noexpand")
                 return (prog1 (next-token/shift ,tsrc ,context)
                          (setf ,token-source ,tsrc))
               else
                 do (setf ,pstate (mex-dispatch ,grp0 ,tsrc ,context nil))
                    (setf ,tsrc (token-source-state ,pstate))
               if (not (parser-value ,pstate))
                 return (progn (setf ,token-source ,tsrc)
                               ,grp0)))))

)


;;; Builtins

(defmacro+ps defbuiltin (name (&rest vars) &body body)
  "Define a builtin command in *COMMAND-TABLE* bound to (COMMAND-KEY NAME).
   BODY is ordinarily that of the expansion handler, but may include code
   for a dynamic pattern, in which case the two parts should be marked by
   :PATTERN and :HANDLER tags inside BODY.  If no matcher is defined, the
   builtin shall have an empty static pattern.
     VARS specify variables to be bound inside BODY: zero or more ``handler
   arguments'' (non-keyword symbols) followed by any of the ``interface
   arguments'' marked by the keywords :CONTEXT, :MATCH, :DISPATCHING,
   :TOKEN-SOURCE, and :SHIP.  Interface arguments are passed in by the code
   that invokes the pattern matcher and the handler, and are visible in both
   functions (except :SHIP, which is only visible in the handler).
   The :CONTEXT argument is the processing context.  The :SHIP argument is
   either null or the shipper function, which may be passed to NEXT-GROUP,
   MEX-DISPATCH, etc. The :MATCH argument is the COMMAND-MATCH object from
   which the matcher and the handler get their input tokens; it should be
   updated by the matcher to hold the right match length, matched token
   count, and token source state.  That, and the token on which the builtin
   got dispatched, may also be bound to the :TOKEN-SOURCE and :DISPATCHING
   arguments.
     Handler arguments are also visible in both functions, but to pass them
   from the pattern matcher to the handler, the pattern matcher must curry
   the handler with the values.  The pattern matcher should return either
   its :MATCH argument, or NIL (if there is no match).  Currying and
   returning can be conveniently done by calling (YIELD <Match> <Value> ...),
   where <Match> is the :MATCH argument.
     The handler should return a parser state with the value true (meaning
   the command is expandable), and accumulating the expansion which may take
   the form of one token or error display, or of a vector of tokens and/or
   groups, or of the boolean true meaning a trivial
   (= empty) expansion."
  (destructuring-bind (handler-args
                       &key ((:match match-var) (ps-gensym "MATCH"))
                            ((:context context-var) (ps-gensym "CTX"))
                            ((:ship ship-var) (ps-gensym "SHIP"))
                            ((:dispatching dispatching-var) nil)
                            ((:token-source token-source-var) nil)
                            (table '*command-table*))
      (let ((interface-vars (member-if #'keywordp vars)))
        (list* (ldiff vars interface-vars) interface-vars))
    (let* ((match-context-declarations `((declare
                                          (ignorable 
                                           ,match-var ,context-var))))
           (ship-declarations `((declare (ignorable ,ship-var))))
           (disp-tsrc-bindings `(,@(when dispatching-var
                                     `((,dispatching-var
                                        (dispatching-token ,match-var))))
                                 ,@(when token-source-var
                                     `((,token-source-var
                                        (token-source-state ,match-var))))))
           (disp-tsrc-declarations (when disp-tsrc-bindings
                                     `((declare
                                        (ignorable
                                         ,@(mapcar #'car
                                             disp-tsrc-bindings))))))
           (docstring (if (stringp (car body)) (pop body)))
           (declarations (loop while (and (listp (car body))
                                          (eq (caar body) 'declare))
                               collect (pop body)))
           (pattern-tag-body (member :pattern body))
           (handler-tag-body (member :handler body))
           (pattern-body (ldiff (cdr pattern-tag-body) handler-tag-body))
           (handler-body (ldiff
                           (if handler-tag-body (cdr handler-tag-body) body)
                           pattern-tag-body))
           (pattern-var (ps-gensym "PTRN"))
           (handler-var (ps-gensym "HNDLR"))
           (cmd (command-key name)))
      `(let ((,pattern-var
              ,(if pattern-body
                   `(lambda (,match-var ,context-var)
                      ,@match-context-declarations
                      (let (,@disp-tsrc-bindings ,@handler-args)
                        ,@disp-tsrc-declarations
                        ,@declarations
                        (flet ((yield (,match-var &rest values)
                                 (setf (parser-value ,match-var)
                                       (apply #'builtin-curry-handler
                                         (parser-value ,match-var)
                                         values))
                                 ,match-var))
                          ,@pattern-body)))
                   `(vector)))
             (,handler-var
              (lambda (,match-var ,context-var ,ship-var ,@handler-args)
                ,@match-context-declarations ,@ship-declarations
                ,@declarations
                ,@(if docstring `(,docstring))
                (block ,name
                  (let (,@disp-tsrc-bindings)
                    ,@disp-tsrc-declarations
                    ,@handler-body)))))
         (add-command
           ,cmd
           (make-builtin :pattern ,pattern-var :handler ,handler-var)
           ,table *adjoin-strong*)))))

(defun builtin-makunbound (name &optional (table *command-table*))
  (let ((cmd (command-key name)))
    (remember cmd table #())))

(defun builtin-curry-handler (builtin &rest args)
  (let ((handler (builtin-handler builtin)))
    (make-builtin
      :handler (lambda (match context ship &rest args*)
                 (apply handler match context ship
                        (append args args*))))))

(defun match-pattern-vars (pattern)
  "Return the list of all variable names in the given MATCH-SETF pattern."
  (loop for part :in pattern
        for head := (and (consp part) (car part))
        if (member head '(? *))
          nconcing (match-pattern-vars (cdr part))
        else if (and (symbolp head) (not (keywordp head)))
          collect head))

(defmacro+ps match-setf ((&rest pattern) token-source context
                         &optional delta-match-length delta-token-count)
  "If the input at the beginning of TOKEN-SOURCE matches the sequence of
   characters, tokens, and/or groups represented by PATTERN, set the
   variables in PATTERN to hold the respective input elements, and evaluate
   to true.  Otherwise, evaluate to null.  If DELTA-MATCH-LENGTH and
   DELTA-TOKEN-COUNT are supplied, count the consumed input.
     An element of PATTERN is either a list of inputs acceptable by
   TOKENS* (which matches an equal sequence of tokens), or a spec of the
   form (<VAR> <TYPE> [<PRED>]), where <VAR> is a symbol (variable name),
   <TYPE> is one of the literal symbols CHAR, TOKEN, GROUP, and <PRED> is an
   optional specifier for a predicate function, quasi defaulting
   to (CONSTANTLY T).  A spec of this form matches/binds a character, token,
   or group of input that satisfies <PRED>."
  (labels ((make-setf (part)
             (destructure/or part
               ((quantifier . quantified)
                (with-ps-gensyms (tsrc-tmp dml-tmp dtc-tmp)
                  (let ((tmp-vars
                          `(,tsrc-tmp
                            ,@(when delta-match-length `(,dml-tmp))
                            ,@(when delta-token-count `(,dtc-tmp))))
                        (save-to-tmp
                          `(setf ,tsrc-tmp ,token-source
                                 ,@(when delta-match-length
                                     `(,dml-tmp ,delta-match-length))
                                 ,@(when delta-token-count
                                     `(,dtc-tmp ,delta-token-count))))
                        (restore-from-tmp
                          `(setf ,token-source ,tsrc-tmp
                                 ,@(when delta-match-length
                                     `(,delta-match-length ,dml-tmp))
                                 ,@(when delta-token-count
                                     `(,delta-token-count ,dtc-tmp))))
                        (match-quantified
                          `(and ,@(mapcar #'make-setf quantified))))
                    (cond
                      ((eq quantifier '?)
                       `(let (,@tmp-vars)
                          (or (progn ,save-to-tmp ,match-quantified)
                              (progn ,restore-from-tmp t))))
                      ((eq quantifier '*)
                       (let* ((quantified-vars
                               (match-pattern-vars quantified))
                              (collector-vars
                               (mapcar (lambda (var)
                                         (ps-gensym (symbol-name var)))
                                       quantified-vars)))
                       `(let (,@tmp-vars
                              ,@(mapcar (lambda (var) `(,var (vector)))
                                        collector-vars))
                          (loop while (progn ,save-to-tmp ,match-quantified)
                                do (setf ,@(mapcan
                                             (lambda (qvar cvar)
                                               `(,cvar
                                                 (vector-add
                                                   ,cvar
                                                   (ensure-vector ,qvar))))
                                             quantified-vars
                                             collector-vars))
                                finally ,restore-from-tmp
                                        (setf ,@(mapcan #'list
                                                  quantified-vars
                                                  collector-vars))
                                        (return t)))))))))
               ((var type &optional pred)
                (when (symbolp var)
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
                    (when extract-op
                      `(and
                         (setf ,var ,extract)
                         ,@(when pred
                             `((funcall ,pred ,var)))
                         ,@(when delta-match-length
                             `((incf ,delta-match-length
                                     ,@(case type
                                         ((group group*)
                                          `((if (group-p ,var)
                                                (group-token-count ,var)
                                                1))))))))))))
               (t (let* ((tokens (ignore-errors (apply #'tokens** part)))
                         (pm (ps-gensym "PM")))
                    (when tokens
                      `(let ((,pm (match-pattern-delimiter
                                    (make-pattern-delimiter :tokens ,tokens)
                                    ,token-source ,context)))
                         (and ,pm
                              (setf ,token-source (token-source-state ,pm))
                              ,@(when delta-match-length
                                  `((incf ,delta-match-length
                                          (delta-match-length ,pm))))
                              ,@(when delta-token-count
                                  `((incf ,delta-token-count
                                          (delta-token-count ,pm)))))))))
               (t (error "Invalid MATCH-SETF pattern part: ~S" part)))))
   `(and ,@(mapcar #'make-setf pattern))))

(defmacro+ps match-setf-update (pattern match context)
  (with-var-value (match)
    `(match-setf ,pattern (token-source-state ,match) ,context
                 (match-length ,match) (matched-token-count ,match))))

(defmacro+ps match-setf-and-yield ((&rest pattern) match context)
  "If the input at the beginning of the TOKEN-SOURCE-STATE in the
   COMMAND-MATCH parser state MATCH matches PATTERN (in the sense of
   MATCH-SETF), set the variables, and evaluate to MATCH whose
   TOKEN-SOURCE-STATE, MATCH-LENGTH, and MATCHED-TOKEN-COUNT have been
   updated accordingly, and whose command has been replaced by one with a
   curried handler.  Otherwise, evaluate to null."
  (with-ps-gensyms (tsrc dml dtc)
    `(let ((,tsrc (token-source-state ,match)) (,dml 0) (,dtc 0))
       (when (match-setf ,pattern ,tsrc ,context ,dml ,dtc)
         (setf (token-source-state ,match) ,tsrc)
         (incf (match-length ,match) ,dml)
         (incf (matched-token-count ,match) ,dtc)
         (yield ,match ,@(match-pattern-vars pattern))))))

(defmacro+ps if-match-bind ((&rest pattern) token-source context
                            seq &optional alt)
  "If the input at the beginning of TOKEN-SOURCE matches PATTERN (in the
   sense of MATCH-SETF), evaluate SEQ in an environment where variables from
   PATTERN are bound to the respective input elements.  If the input doesn't
   match, and ALT is provided, evaluate it instead."
  `(let (,@(match-pattern-vars pattern))
     (if (match-setf ,pattern ,token-source ,context) ,seq ,alt)))


;;; Local Variables: ***
;;; mode:lisp ***
;;; local-font-lock-keywords:(("(\\(def\\(?:\\(ps\\(?:macro\\|fun\\)\\|\\(?:un\\|macro\\)\\+ps\\|builtin\\)\\|\\(guard\\|enum\\)\\|\\(struct-guarded\\)\\)\\)[ \t\n]+(?\\([^()]*?\\)[ \t\n]" (1 font-lock-keyword-face) (5 (cond ((match-beginning 2) font-lock-function-name-face) ((match-beginning 3) font-lock-variable-name-face) ((match-beginning 4) font-lock-type-face)))) ("(\\(ambi-ps\\)\\_>" (1 font-lock-preprocessor-face)) "\\_<if-match-bind\\_>") ***
;;; End: ***
