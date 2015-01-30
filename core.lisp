(in-package #:mex)

;;; Core Mex builtins

(ambi-ps ()

  (defvar ellipsis (tokens% :chars "...")
    "Tokens representing ellipsis in reconstructed faulty input of error
     displays.")

  (defbuiltin relax (:token-source tsrc)
    "Consume nothing, expand to nothing."
    (parser-expansion-state tsrc t))

  (defbuiltin comment (:match match :context ctx)
    "Consume tokens up to and including the end of the line, expand to
     nothing."
    :pattern
    (loop for tok := (next-token/shift (token-source-state match) ctx)
          until (or (newline-token-p tok) (eot-p tok))
          do (incf (match-length match))
          finally (return match))
    :handler
    (parser-expansion-state (token-source-state match) t))

#|
@BEGIN TEST COMMENT
@MEX
\comment: This is a comment
\setcat+\active% \def%{\comment}
% Now, this too is a comment
@JSON
{"t": " \n"}
@END TEST
|#

  (defbuiltin expandafter (tok1 tok2
                           :match match :dispatching dispatching
                           :context ctx)
    "Consume two tokens.  If the second token (subject to parameter
     expansion) is a command or an active, expand it (consuming more input
     if necessary), and expand to the command's expansion preceded by the
     first token.  Otherwise, expand to just the two tokens."
    :pattern
    (match-setf-and-yield ((tok1 token #'non-eot-p)
                           (tok2 token* #'non-eot-p))
        match ctx)
    :handler
    (parser-state-bind (:value expd :error error :token-source tsrc+)
        (mex-dispatch tok2 (token-source-state match) ctx nil)
      (if error
          (parser-error-state* tsrc+
            :add-to error :prepend-input dispatching tok1)
          (parser-expansion-state
            (let ((ectx (and tsrc+ (expansion-context tsrc+))))
              (expansion-to-token-source
                (if expd (vector tok1) (vector tok1 tok2))
                tsrc+ ectx))
            t))))

  (defbuiltin expandafter (:token-source tsrc)
    "If the input doesn't have two tokens after \\expandafter, consume
     nothing and expand to nothing."
    (parser-expansion-state tsrc t))

  (defun replace-context (token context match)
    "Helper function for the builtins \\global, \\local, and \\parent.
     Return a parser state for tokens after MATCH, with the accumulator
     holding a copy of TOKEN whose native context has been replaced by
     CONTEXT."
    (if context
        (let ((replacement-token (copy-structure token)))
          (setf (token-context replacement-token) context)
          (parser-expansion-state (token-source-state match)
            replacement-token))
        (parser-error-state* (token-source-state match)
          "noContextInput" (dispatching-token match) token)))

  (defbuiltin global (tok :match match :context ctx)
    "Consume one token.  Expand to a token which is just like the consumed
     but whose native context is the global (top-level) context."
    :pattern
    (match-setf-and-yield ((tok token #'non-eot-token-p)) match ctx)
    :handler 
    (let ((global-ctx
           (loop for octx := (ensure-opaque-context ctx)
                          :then (parent-context octx)
                 with gctx := nil
                 while octx
                 if (or *no-transparent-contexts* (opaque-context-p octx))
                   do (setf gctx octx)
                 finally (return gctx))))
      (replace-context tok global-ctx match)))

  (defbuiltin local (tok :match match :context ctx)
    "Consume one token.  Expand to a token which is just like the consumed
     but whose native context is the local context (group or top-level)."
    :pattern
    (match-setf-and-yield ((tok token #'non-eot-token-p)) match ctx)
    :handler
    (replace-context tok ctx match))

  (defbuiltin parent (tok :match match :context ctx)
    "Consume zero or more additional \\parent tokens, followed by one other
     token.  Expand to a token which is just like the last one consumed but
     whose native context is the parent (the parent of the parent, etc.) of
     the local context (group), or of its nearest opaque ancestor."
    :pattern
    (loop for tok := (next-token/shift (token-source-state match) ctx)
          while (token-is tok :command "parent")
          do (incf (match-length match))
             (incf (matched-token-count match))
          finally (when (non-eot-token-p tok)
                    (incf (match-length match))
                    (return (yield match tok))))
    :handler
    (loop for i :from 0 :to (matched-token-count match)
          for dtoks := (ensure-vector (dispatching-token match))
                 :then (spliced dtoks 0 0 (dispatching-token match))
          with pctx := (ensure-opaque-context ctx)
          while pctx do (setq pctx (parent-context pctx))
          finally (setf (dispatching-token match) dtoks)
                  (return (replace-context tok pctx match))))

#|
@BEGIN TEST REPLACE-CONTEXT
@MEX
\def\loc{outer} {\def\loc{middle} {\def\loc{inner}
\loc{} vs. \global\loc{} vs. \parent\loc{} vs. \parent\parent\loc{}}}
@JSON
{"t": "  \ninner vs. outer vs. middle vs. outer"}
@END TEST

@BEGIN TEST REPLACE-CONTEXT-FOR-DEF
@MEX
\def\loc{outer}
{\def\loc{inner} \global\def\getloc{\loc}
\global\def\redefloc{\local\def\loc{\loc}}}
\getloc{} vs. \loc{} vs. \redefloc{}\loc{}
@JSON
{"t": "\n \n\ninner vs. outer vs. inner"}
@END TEST
|#

  (defbuiltin newlang (lcid proto-lcid
                       :match match :context ctx
                       :token-source tsrc :dispatching dispatching)
    "Consume <Lang>[\\like<Proto>], where <Lang> and <Proto> are groups
     whose string representations after complete expansion should
     name languages.  Switch the locale of CONTEXT to the language <Lang>,
     creating the table if it doesn't exit (reuse definitions from language
     <Proto>'s table); expand to nothing."
    :pattern
    (match-setf-and-yield ((lcid group #'group-p)
                           (? (:command "like")
                              (proto-lcid group #'group-p)))
        match ctx)
    :handler
    (let ((locale-id (input-to-string lcid))
          (proto-id (and proto-lcid (input-to-string proto-lcid))))
      (if (and (not (equal locale-id ""))
               (not (equal proto-id ""))
               (add-context-locale ctx locale-id proto-id))
          (parser-expansion-state tsrc t)
          (let ((ed (error-display* "invalidLocale" dispatching lcid)))
            (parser-error-state tsrc
              (if proto-lcid
                  (error-display* :add-to ed
                    :append-input (tokens% :command "like") proto-lcid)
                  ed))))))

  (defbuiltin setlang (lcid :match match :context ctx
                            :token-source tsrc :dispatching dispatching)
    "Consume one group (subject to parameter expansion) whose string
     representation should name a language.  Switch the locale of CONTEXT
     and all ancestor contexts to this language; expand to nothing."
    :pattern
    (match-setf-and-yield ((lcid group* #'group-p)) match ctx)
    :handler
    (let ((locale-id (input-to-string lcid)))
      (if (and (not (equal locale-id ""))
               (set-context-locale ctx locale-id))
          (parser-expansion-state tsrc t)
          (parser-error-state* tsrc "invalidLocale" dispatching lcid))))

#|
@BEGIN TEST MULTILANG
@MEX
\setcat\letter+\constituent é
\newlang{fr_FR}\lcdef\bonté{Toute autre science est dommageable à celui qui n'a pas la science de la bonté.}
\newlang{ru_RU}\lcdef\bonté{Тому, кто не постиг науки добра, всякая иная наука приносит лишь вред.}
\setlang{en_US}\lcdef\bonté{All other knowledge is hurtful to him who has not the science of goodness.}
\setlang{ru_RU}\bonté
\newlang{et_EE}\bonté
\newlang{fr_CH}\like{fr_FR}\bonté
@JSON
{"t": "\n\n\n\n\u0422\u043e\u043c\u0443, \u043a\u0442\u043e \u043d\u0435 \u043f\u043e\u0441\u0442\u0438\u0433 \u043d\u0430\u0443\u043a\u0438 \u0434\u043e\u0431\u0440\u0430, \u0432\u0441\u044f\u043a\u0430\u044f \u0438\u043d\u0430\u044f \u043d\u0430\u0443\u043a\u0430 \u043f\u0440\u0438\u043d\u043e\u0441\u0438\u0442 \u043b\u0438\u0448\u044c \u0432\u0440\u0435\u0434.\nAll other knowledge is hurtful to him who has not the science of goodness.\nToute autre science est dommageable \u00e0 celui qui n'a pas la science de la bont\u00e9."}
@END TEST
|#

  (defbuiltin csname (:match match :context ctx :dispatching dispatching)
    "Consume <Content>\\endcsname, where <Content> is any non-empty sequence
     of tokens not containing paragraph breaks.  Expand commands and
     parameters in <Content> and obtain the string representation of the
     expansion.  Expand to a command token with that string for name."
    :pattern
    (loop for tok := (next-token/shift (token-source-state match) ctx)
          if (or (par-break-p tok) (eot-p tok))
            return nil
          else if (token-is tok :command "endcsname")
            if (> (match-length match) 0)
              do (setf (accumulator match) (ensure-vector content)
                       (terminator match) tok)
                 (incf (match-length match))
                 (incf (matched-token-count match))
              and return match
            else
              return nil
            end
          else
            collect tok :into content
            and do (incf (match-length match)))
    :handler
    (let* ((content (accumulator match))
           (tsrc (token-source-state match))
           (expansion-pstate (get-full-expansion content tsrc ctx nil)))
      (if (parser-error expansion-pstate)
          (parser-error-state* tsrc
            :add-to (parser-error expansion-pstate)
            :prepend-input dispatching
            :append-input (terminator match)
            :sep ellipsis)
          (parser-accumulator-state tsrc
            (make-command-token
              :start (token-start dispatching)
              :end (token-end (terminator match))
              :context (token-context dispatching)
              :name (input-to-string (accumulator expansion-pstate))
              :escape-chr #\\)))))

#|
@BEGIN TEST CSNAME
@MEX
\def\fire{char}\def\wood{coal}
\def\charcoal{ashes}
\csname\fire\wood\endcsname
@JSON
{"t": "\n\nashes"}
@END TEST
|#

  (defun match-definition (match context no-pattern)
    "Pattern matcher function for the builtins \\def, \\lcdef, \\edef and
     \\alias.  Parse input after MATCH for a macro / alias definition.
     Curry the handler of the command in MATCH with 1) the command or active
     token to be defined, and 2) the expansion, if NO-PATTERN is true, or a
     MACRO object linking the pattern and the expansion, if it is false."
    (if-match-bind ((cmd-tok token #'dispatching-token-p))
        (token-source-state match) context
      (progn
        (incf (match-length match))
        (loop named parse-def
          with delimiter := nil
          with param := nil
          with expn := nil
          do (setf delimiter nil param nil expn nil)
             (loop
               with d-tok := nil
               do (if-match-bind ((input group))
                      (token-source-state match) context
                    (cond ((group-p input)
                           (setf expn (group-contents input))
                           (incf (match-length match)
                                 (group-token-count input)))
                          ((and no-pattern (non-eot-p input))
                           (setf expn (vector input))
                           (incf (match-length match)))
                          ((param-token-p input)
                           (setf param input)
                           (incf (match-length match)))
                          (t (setf d-tok input)
                             (incf (match-length match)))))
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
                 do (setf (parser-error match)
                            (error-display*
                              :add-to d-tok :append-message "unfinishedDef"
                              :prepend-input (dispatching-token match)
                                             cmd-tok
                                             (pattern-tokens pattern))
                          (parser-value match)
                            (builtin-curry-handler (parser-value match)
                              cmd-tok nil))
                    (return-from parse-def match)
               else
                 collect d-tok :into d-toks)
          when delimiter
            collect delimiter :into pattern
          when param
            collect param :into pattern
          when expn
            do (setf (parser-value match)
                       (builtin-curry-handler (parser-value match)
                         cmd-tok
                         (if no-pattern
                             expn
                             (make-macro
                               :pattern (ensure-vector pattern)
                               :expansion expn))))
            and return match))))

  (defbuiltin def (cmd-tok macro
                   :match match :context ctx :dispatching dispatching
                   :token-source tsrc)
    "Consume <Command><Pattern><Expansion>, where <Command> is a command or
     active character token, <Pattern> is a (possibly empty) sequence of
     tokens which may not include braces or paragraph breaks, and
     <Expansion> is a group.  Obtain the key of <Command>, split <Pattern>
     into a sequence of alternating parameter tokens and delimiters of
     literal tokens, create a macro matching that sequence and expanding to
     the contents of <Expansion>, and store the macro in the command table
     of the native context of the dispatching token (or its nearest opaque
     ancestor) under the given command key.  Expand to nothing."
    (declare (ignorable cmd-tok macro))
    :pattern
    (match-definition match ctx nil)
    :handler
    (if (parser-error match)
        match
        (let ((octx (ensure-opaque-context (token-context dispatching))))
          (if octx
              (bind/init ((table (command-table octx) (make-table)))
                (add-command (token-command-key cmd-tok) macro table
                             *adjoin-strong*)
                (parser-expansion-state tsrc t))
              (parser-error-state* tsrc
                "noContextInput" dispatching cmd-tok)))))

  (defbuiltin lcdef (cmd-tok macro
                     :match match :context ctx :dispatching dispatching
                     :token-source tsrc)
    "Like \\def, except that the macro is stored in the localized command
     table for the current locale language (cf. \\setlang, \\newlang)."
    (declare (ignorable cmd-tok macro))
    :pattern
    (match-definition match ctx nil)
    :handler
    (if (parser-error match)
        match
        (let ((octx (ensure-opaque-context (token-context dispatching))))
          (if octx
              (bind/init ((locale-table
                            (locale-table octx)
                            (make-locale-table (context-locale octx))))
                (let ((command-table (lookup "" locale-table)))
                  (if command-table
                      (let ((key (token-command-key cmd-tok)))
                        (add-command key macro command-table
                                     *adjoin-strong*)
                        (parser-expansion-state tsrc t))
                      (parser-error-state* tsrc
                        "noLocale" dispatching cmd-tok))))
              (parser-error-state* tsrc
                "noContextInput" dispatching cmd-tok)))))

  (defbuiltin edef (cmd-tok macro
                    :match match :context ctx :dispatching dispatching
                    :token-source tsrc)
    "Consume <Command><Pattern><Expansion>, where <Command> is a command or
     active character token, <Pattern> is a (possibly empty) sequence of
     tokens which may not include parentheses or paragraph breaks, and
     <Expansion> is a group.  Obtain the complete expansion of the contents
     of <Expansion>, except that parameter tokens which occur in <Pattern>
     should be left unexpanded.  Record a macro in the same way as \\def,
     but use the expanded <Expansion> as the expansion of the macro."
    (declare (ignorable cmd-tok macro))
    :pattern
    (match-definition match ctx nil)
    :handler
    (if (parser-error match)
        match
        (let ((octx (ensure-opaque-context (token-context dispatching))))
          (if octx
              (let* ((params (loop for part :across (command-pattern macro)
                                   if (param-token-p part) collect part))
                     (content (macro-expansion macro)))
                (parser-state-bind (:accumulator expn :error expn-errors)
                    (let ((*unexpandable-params*
                           (append params *unexpandable-params*)))
                      (get-full-expansion content tsrc ctx t))
                  (if expn-errors
                      (parser-error-state* tsrc
                        :add-to expn-errors
                        :prepend-message "errorsInAliasOrEdef"
                        :prepend-input dispatching cmd-tok
                        :sep ellipsis)
                      (bind/init ((table (command-table octx)
                                         (make-table)))
                        (setf (macro-expansion macro)
                              (ensure-vector expn))
                        (add-command (token-command-key cmd-tok) macro table
                                     *adjoin-strong*)
                        (parser-expansion-state tsrc t)))))
              (parser-error-state* tsrc
                "noContextInput" dispatching cmd-tok)))))

#|
@BEGIN TEST DEF-VS-EDEF
@MEX
\def\foo{outer}
{\def\foo{inner}\parent\def\bydef{\local\foo}\parent\edef\byedef{\local\foo}}
\bydef{} vs. \byedef{}
@JSON
{"t": "\n\nouter vs. inner"}
@END TEST

@BEGIN TEST EDEF-WITH-PARAMS
@MEX
\def\deffoobar#\X#\Y{\global\edef\foo#\Y{foo: <#\X> <#\Y>}\
  \global\def\bar{bar: <#\Y>}}
\deffoobar{one}{two}\def\Y{why?}
\foo{five} ; \bar{}
@JSON
{"t": "\n \nfoo: <one> <five> ; bar: <why?>"}
@END TEST
|#

  (defbuiltin alias (cmd-tok content
                     :match match :context ctx :dispatching dispatching
                     :token-source tsrc)
    "Consume <Command><Expansion>, where <Command> is a command or active
     character token, and <Expansion> is a group (or a singleton token).
     Obtain the key of <Command> and the complete expansion of the contents
     of <Expansion>, and store it in the alias table of the native context
     of the dispatching token (or its nearest opaque ancestor) under the
     given command key.  Expand to nothing."
    (declare (ignorable cmd-tok content))
    :pattern
    (match-definition match ctx t)
    :handler
    (if (parser-error match)
        match
        (let ((octx (ensure-opaque-context (token-context dispatching))))
          (if octx
              (parser-state-bind (:accumulator expn :error expn-errors)
                  (get-full-expansion content tsrc ctx nil)
                (if expn-errors
                    (parser-error-state* tsrc
                      :add-to expn-errors
                      :prepend-message "errorsInAliasOrEdef"
                      :prepend-input dispatching cmd-tok
                      :sep ellipsis)
                    (bind/init ((alias-table (alias-table octx)
                                             (make-table)))
                      (remember (token-command-key cmd-tok)
                                alias-table (ensure-vector expn))
                      (parser-expansion-state tsrc t))))
              (parser-error-state* tsrc
                "noContextInput" dispatching cmd-tok)))))

#|
@BEGIN TEST ALIAS
@MEX
\def\foo{Foo!}
{\alias\emansc{\noexpand\noexpand\noexpand\endcsname} \csname foo\emansc}
{\def\emansc{\endcsname} \csname foo\emansc}
@JSON
[{"t": "\n Foo!\n "},
 @ERROR IN "\\csname" ("No matching definition"),
 {"t": "foo"},
 @ERROR IN "\\endcsname" ("Undefined command")]
@END TEST
|#

  (defbuiltin error (faulty-input message
                     :match match :context ctx :dispatching dispatching
                     :token-source tsrc)
    "Consume <Faulty><Message>, two groups or singleton tokens.  Expand to
     an error display with faulty input being the first group or token
     (subject to parameter expansion), and with error message being the
     string representation of the full expansion of the second group or
     token."
    :pattern
    (match-setf-and-yield ((faulty-input group* #'non-eot-p)
                           (message group #'non-eot-p))
        match ctx)
    :handler
    (flet ((errors-in-error (ed)
             (parser-error-state* (token-source-state match)
               :add-to ed
               :prepend-message "errorsInError"
               :prepend-input dispatching faulty-input message)))
      (cond
        ((parser-error match)
         (errors-in-error (parser-error match)))
        ((error-display-p faulty-input)
         (errors-in-error faulty-input))
        ((error-display-p message)
         (errors-in-error message))
        (t (parser-state-bind (:accumulator msg :error expn-errors)
               (get-full-expansion message tsrc ctx nil)
             (if (or expn-errors (false-p (setf msg (input-to-string msg))))
                 (errors-in-error expn-errors)
                 (parser-error-state tsrc
                   (make-error-display
                     :faulty-input (if (group-p faulty-input)
                                       (group-contents faulty-input)
                                       (ensure-vector faulty-input))
                     :message (vector msg)))))))))

#|
@BEGIN TEST ERROR
@MEX
\def\foo{Foo!}
\error{\foo=#\foo}{Worse than a crime}
@JSON
[{"t": "\n"},
 @ERROR IN "\\foo=Foo!" ("Worse than a crime")]
@END TEST
|#

  (defun match-ccat-spec (match context)
    "Pattern match function for the builtins \\setcat, \\chr, etc.
     Parse input after MATCH for a character category specification.  Curry
     the handler of the command in MATCH with 1) the category mask, and 2)
     the boolean true if the specification sets the base category, or false
     if it just toggles active and constituent flags."
    (let ((cat 0) (base nil))
      (macrolet ((set-base (cmd value)
                   (let ((cmd-name (symbol-to-js-string cmd)))
                     `(if-match-bind ((:command ,cmd-name))
                          (token-source-state match) context
                        (unless base
                          (incf (match-length match))
                          (setf cat ,value base t)))))
                 (set-flag (op cmd compose value)
                   (let ((cmd-name (symbol-to-js-string cmd)))
                     `(if-match-bind ((:chars ,(string op)
                                       :command ,cmd-name))
                          (token-source-state match) context
                        (progn (incf (match-length match) 2)
                               (setf cat (,compose cat ,value))
                               t)))))
        (when (command-token-p
                (next-token* (token-source-state match) context))
          (or (set-base invalid *ccat-invalid*)
              (set-base whitespace *ccat-whitespace*)
              (set-base newline *ccat-newline*)
              (set-base escape *ccat-escape*)
              (set-base param *ccat-param*)
              (set-base lbrace *ccat-lbrace*)
              (set-base rbrace *ccat-rbrace*)
              (set-base letter *ccat-letter*)
              (set-base number *ccat-number*)
              (set-base other *ccat-other*)
              (set-base active *ccat-active*)
              (set-base constituent *ccat-constituent*)))
        (loop
          while (or (set-flag + active logior *ccat-active*)
                    (set-flag - active logandc2 *ccat-active*)
                    (set-flag + constituent logior *ccat-constituent*)
                    (set-flag - constituent logandc2 *ccat-constituent*)))
        (setf (parser-value match)
                (builtin-curry-handler (parser-value match) cat base))
        match)))

  (defbuiltin setcat (cat set-base c
                      :match match :context ctx :dispatching dispatching
                      :token-source tsrc)
    "Consume some tokens that constitute a character category specification,
     and one untokenized character.  Replace the category of the character
     in the category table of the native context of the dispatching
     token (or its nearest opaque ancestor) by the specified category.
     Expand to nothing."
    (declare (ignorable cat set-base))
    :pattern
    (and (setf match (match-ccat-spec match ctx))
         (match-setf-and-yield ((c char)) match ctx))
    :handler
    (let ((octx (ensure-opaque-context (token-context dispatching))))
      (if octx
          (let ((ctab (category-table octx)))
            (unless set-base
              (setf cat (logior (ccat-base (char-cat c ctab)) cat)))
            (setf (category-table octx) (char-cat c ctab cat))
            (parser-expansion-state tsrc t))
          (parser-error-state* tsrc "noContextInput" dispatching))))

  (defbuiltin chr (cat set-base c
                   :match match :context ctx :dispatching dispatching
                   :token-source tsrc)
    "Consume some tokens that constitute a character category specification,
     and one untokenized character.  Expand to one character token with the
     given character value and specified category."
    (declare (ignorable cat set-base))
    :pattern
    (and (setf match (match-ccat-spec match ctx))
         (match-setf-and-yield ((c char)) match ctx))
    :handler
    (let* ((tctx (token-context dispatching))
           (ctab (if tctx (category-table tctx) *category-table*))
           (pos (char-source-offset tsrc)))
      (unless set-base
        (setf cat (logior (ccat-base (char-cat c ctab)) cat)))
      (parser-expansion-state tsrc
        (guarded-make-char-token
          :start (and pos (1- pos)) :end pos :context ctx
          :chr c :category cat))))

#|
@BEGIN TEST REPLACE-CCAT
@MEX
\setcat\lbrace[\setcat\rbrace]
[\setcat\letter+\constituent{\setcat\letter+\constituent}\def\{}[{Foo!}] \{}]
{\setcat+\active@\global\def@{Bar!}} \chr+\active@ \carriagereturn\linefeed
@JSON
{"t": "\n {Foo!}\n Bar! \r\n"},
@END TEST
|#

)


;;; Local Variables: ***
;;; mode:lisp ***
;;; local-font-lock-keywords:(("(\\(def\\(?:\\(ps\\(?:macro\\|fun\\)\\|\\(?:un\\|macro\\)\\+ps\\|builtin\\)\\|\\(guard\\|enum\\)\\|\\(struct-guarded\\)\\)\\)[ \t\n]+(?\\([^()]*?\\)[ \t\n]" (1 font-lock-keyword-face) (5 (cond ((match-beginning 2) font-lock-function-name-face) ((match-beginning 3) font-lock-variable-name-face) ((match-beginning 4) font-lock-type-face)))) ("(\\(ambi-ps\\)\\_>" (1 font-lock-preprocessor-face)) "\\_<if-match-bind\\_>") ***
;;; End: ***
