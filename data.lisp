(in-package #:mex)

;;; Document stack and data registers

(ambi-ps ()

  ;;; Stack

  (defstruct (dom-stacks (:conc-name))
    "The DOM multi-stack.  ELEMENT-STACK represents a tower of nested open
     DOM-ELEMENTS.  DOM-ELEMENT, DOM-TEXT and DOM-COMMENT objects placed on
     CURRENT-STACK become successive child nodes of the innermost open
     DOM-ELEMENT.  TOKENs are packed to DOM-TEXT objects which are handled
     in the general manner.  DOM-ATTRIBUTE objects become the element's
     attributes, all subject to verification.  DOM-RECIPE objects may be
     used to invoke dynamic behviour on the client side.  Objects of other
     types may be placed on the stack but are discarded when the ."
     current-stack element-stack id-counter)

  (defstruct (dom-element (:conc-name element-))
    "A DOM element.  CONTENT is a stack which may replace CURRENT-STACK in a
     DOM-STACKS object."
    name attributes content)

  (defstruct (dom-text (:conc-name text-))
    "A DOM text node.  CONTENT is a string."
    content)

  (defstruct (dom-comment (:conc-name comment-))
    "A DOM comment node."
    content)

  (defstruct (dom-recipe (:conc-name recipe-))
    "An object which should invoke some dynamic behaviour identified by
     HANDLER-NAME, acting on DATA, when the DOM structure is rendered
     client-side."
    handler-name data)

  (defmacro element (name &rest content)
    "A more concise DOM-ELEMENT constructor syntax."
    (loop for content :on content :by #'cddr
          for (attr val) := content
          while (keywordp attr)
          collect (string-downcase (symbol-name attr)) :into attrs
          collect val :into attrs
          finally (return
                    `(make-dom-element
                       :name ,name
                       :attributes (make-table ,@attrs)
                       :content (vector ,@content)))))

  (defvar *dom-root-wrapper*
    (lambda (&optional content)
      (let ((elt (element "DIV" :class "mex_wrapper")))
        (setf (element-content elt) (ensure-vector content))
        elt))
    "The value is a function returning a DOM-ELEMENT that Mex should put at
     the root of the DOM multi-stack.  The function should accept one
     optional argument (an input or vector of inputs), but may ignore it.")

  (defun init-dom-stack (context)
    (loop for ctx := context :then (parent-context ctx)
          while ctx
          for stacks = (dom-stack ctx)
          until stacks
          with gctx := nil
          with rctx := nil
          if (opaque-context-p ctx) do (setf gctx ctx)
          do (setf rctx ctx)
          finally (unless stacks
                    (let ((relt (funcall *dom-root-wrapper*)))
                      (setf stacks
                              (make-dom-stacks
                                :current-stack (copy-stack
                                                 (element-content relt))
                                :element-stack (stack relt)
                                :id-counter 0)))
                    (unless *no-transparent-contexts*
                      (setf rctx gctx))
                    (when (and rctx (not (eq rctx context)))
                      (setf (dom-stack rctx) stacks)))
                  (return stacks)))

  (defmacro with-context-dom-stack ((var context &key elements current id)
                                    &body body)
    "Evaluate BODY with VAR bound to the DOM multistack associated with
     CONTEXT. If necessary, initialize the stack by opening a root DIV."
    `(bind/init ((,var (dom-stack ,context) (init-dom-stack ,context)))
       ,@(if (or elements current id)
             `((let (,@(if elements `((,elements (element-stack ,var))))
                     ,@(if current `((,current (current-stack ,var))))
                     ,@(if id `((,id (id-counter ,var)))))
                 (prog1 (progn ,@body)
                   (setf ,@(if elements `((element-stack ,var) ,elements))
                         ,@(if current `((current-stack ,var) ,current))
                         ,@(if id `((id-counter ,var) ,id))))))
             body)))

  (defun text-node (input)
    "A more concise DOM-TEXT constructor syntax."
    (make-dom-text
      :content (if (stringp input) input (input-to-string input))))

  (defun recipe (handler data)
    "A more concise DOM-RECIPE constructor syntax."
    (make-dom-recipe :handler-name handler :data data))

  (defun dom-stack-next-id (stacks &optional (prefix "Mex"))
    "Generate and return a new id which is guaranteed to differ from all ids
     previously generated for STACKS."
    (bind/init ((n (id-counter stacks) 1))
      (prog1 (interpolate "#{prefix}#{n}")
        (setf (id-counter stacks) (1+ n)))))

  (defun dom-stack-open-element (element stacks)
    "Push a new open element ELT onto the element stack in STACKS.  If ELT
     has some content, copy it onto the current stack, otherwise, leave the
     current stack empty."
    (let ((next-element (stack-peek (element-stack stacks))))
      (when next-element
        (setf (element-content next-element)
                (ensure-vector (current-stack stacks)))))
    (setf (current-stack stacks)
            (if (element-content element)
                (copy-stack (element-content element))
                (make-stack)))
    (stack-push element (element-stack stacks))
    element)

  (defun dom-stack-close-element (stacks)
    "Close, pop, and return the most recently open element on STACKS.
     TOKENs placed on the current stack are packed to DOM-TEXT; these and
     other DOM objects are then verified and replace the content of the
     element.  If verification fails, an ERROR-DISPLAY is returned instead
     of the closed object."
    (let ((elt-stack (element-stack stacks)))
      (unless (stack-empty-p elt-stack)
        (let* ((closed-element (stack-pop elt-stack))
               (next-element (stack-peek elt-stack))
               (content (verify-element-content
                          (ensure-vector (current-stack stacks))
                          closed-element stacks))
               (ret-element (if (error-display-p content)
                                content
                                (progn
                                  (setf (element-content closed-element)
                                          content)
                                  closed-element))))
          (when next-element
            (stack-push
              ret-element
              (setf (current-stack stacks)
                    (copy-stack (element-content next-element)))))
          ret-element))))

  (defun verify-element-content (content parent-element stacks)
    "Ensure that CONTENT is valid as the content of PARENT-ELEMENT, that is:
      * merge all consecutive runs of tokens into DOM-TEXT nodes;
      * convert error displays to their DOM-ELEMENT representations; 
      * remove everything else which is not DOM stuff.
     Return validated content as a vector of DOM-ELEMENT, DOM-TEXT,
     DOM-COMMENT and/or DOM-RECIPE data."
    (declare (ignore parent-element))
    (let ((validated (make-stack)))
      (loop for i :from 0 :to (length content)
            for elt := (aref* content i)
            with text-node := nil
            if (token-p elt)
              if (not text-node)
                do (setf text-node (make-dom-text :content (make-stack)))
                end
              and do (stack-push (input-to-string elt)
                                 (text-content text-node))
            else
              if text-node
                do (setf (text-content text-node)
                           (ensure-string (text-content text-node)))
                   (stack-push text-node validated)
                   (setf text-node nil)
                end
              and if (or (dom-element-p elt)
                         (dom-comment-p elt)
                         (dom-text-p elt)
                         (dom-recipe-p elt))
                do (stack-push elt validated)
              else if (error-display-p elt)
                do (let ((id (dom-stack-next-id stacks "MexError")))
                     (stack-push (error-display-to-dom elt id)
                                 validated)))
      #| TBD: verification according to document type |#
      validated))

  (defvar *dom-error-wrapper*
    (lambda (input eotp messages id)
      (element "SPAN"
        :class "mex_error_display" :id id
        (text-node input)
        (when eotp (element "SPAN" :class "mex_eot"))
        (let ((msg-elt (element "SPAN" :class "mex_error_messages")))
          (setf (element-content msg-elt)
                  (ensure-vector
                    (loop for msg :across messages
                          for sep := nil then t
                          if sep collect (element "BR")
                            and collect (element "BR")
                          collect (element "SPAN" (text-node msg)))))
          msg-elt)))
    "The value is a function returning a DOM-ELEMENT that Mex should
     substitute for an ERROR-DISPLAY.  The function should accept four
     arguments: a string representing the faulty input, a boolean indicating
     whether the faulty input included an end of text, a sequence of strings
     representing the error messages, and an id for the wrapper element.")

  (defun error-display-to-dom (error-display id)
    "Return a DOM-ELEMENT representing the error encoded by ERROR-DISPLAY."
    (declare (special *root-context*))
    (loop
      for msg :across (error-display-message error-display)
      with str with error with errors
      do (if (stringp msg)
             (setf str msg error nil)
             (let ((msg-tsrc (simulate-command-with-tokens
                               "errmsg" msg nil *root-context* nil)))
               (if msg-tsrc
                   (let ((pst (get-group-string msg-tsrc *root-context*)))
                     (setf str (parser-value pst)
                           error (parser-error pst)))
                   (setf str nil error t))))
      if error
        do (setf errors t)
      else
        collect str :into msgs
      finally
        (let ((msgs0 (ensure-vector msgs))
              (faulty-input (error-display-faulty-input error-display)))
          (return
            (funcall *dom-error-wrapper*
              (input-to-string faulty-input)
              (loop for i :from (1- (length faulty-input)) :downto 0
                    if (eot-p (aref faulty-input i)) return t)
              (if errors
                  (if (= (length msgs0) 0)
                      (vector "Internal error while displaying errors")
                      (spliced msgs0 (length msgs0) 0
                        #.(concatenate 'string
                            "Additionally, some errors could not be"
                            " displayed due to internal error")))
                  msgs0)
              id)))))

  (defmacro element-attr-bind ((&rest bindings) element &body body)
    (with-ps-gensyms (attrs)
      (flet ((attr-name (x) (string-downcase (string x))))
        `(let* ((,attrs (element-attributes ,element))
                ,@(loop
                    for binding :in bindings
                    for (var . name)
                      := (if (symbolp binding)
                             (cons binding (attr-name binding))
                             (destructuring-bind (var &optional nm) binding
                               (cons var (or nm (attr-name var)))))
                    collect `(,var (lookup ,name ,attrs))))
           ,@body))))

  (defmethod data-to-input ((element dom-element) context)
    (element-attr-bind (id (cls "class")) element
      (let ((elt-name (element-name element))
            (hash-id (if* id (interpolate "###{id}") ""))
            (dot-cls (if* cls (interpolate ".#{cls}") "")))
        (string-to-input (interpolate "<#{elt-name}#{hash-id}#{dot-cls}/>")
                         context *plain-category-table*))))

  (defmethod data-to-input ((text-node dom-text) context)
    (string-to-input (text-content text-node)
                     context *plain-category-table*))

  (defmethod data-to-input ((comment dom-comment) context)
    (let ((content (comment-content comment)))
      (string-to-input (interpolate "<!-- #{content} --/>")
                       context *plain-category-table*)))

  (defmethod data-to-input ((recipe dom-recipe) context)
    (let ((name (recipe-handler-name recipe))
          (data-json (json:encode-json-to-string (recipe-data recipe))))
      (string-to-input (interpolate "<? recipe #{name} #{data-json} ?>")
                       context *plain-category-table*)))

  (defun data-to-input* (data context)
    "Like DATA-TO-INPUT, but with support for vectors and booleans."
    (if (vectorp data)
        (loop for sub :across data
              for input := (data-to-input* sub context)
              for inputs := (ensure-vector input)
                :then (if (vectorp input)
                          (vector-add inputs input)
                          (spliced inputs (length inputs) 0 input))
              finally (return inputs))
        (data-to-input
          (cond ((true-p data) "true") ((false-p data) "false") (t data))
          context)))


  ;;; Registers

  (defun context-stack-push (value context)
    "Dispose of datum VALUE by pushing it to DOM stack associated with
     CONTEXT."
    (with-context-dom-stack (stacks context)
      (let ((current (current-stack stacks)))
        (if (and (vectorp value) (token-p (aref* value 0)))
            (loop for tok :across value do (stack-push tok current))
            (stack-push value current)))))

  (defvar *put-data-callback* nil
    "A function to dispose of data objects obtained from input or in the
     course of computation, or NIL the data should be pushed to DOM stack.")

  (defun put-data (value context)
    "Dispose of datum VALUE obtained from input or in the course of
     computation."
    (if *put-data-callback*
        (funcall *put-data-callback* value context)
        (context-stack-push value context)))

  (defun add-register-commands (key command-table register-table)
    "Add to COMMAND-TABLE two builtin commands to store and retrieve the
     value under KEY in REGISTER-TABLE.  More precisely, if <RegToken> is
     the command token corresponding to the given KEY, and <ValueTokens> is
     some token sequence constituting a command that disposes of a value,
     then bare <RegToken> shall be a command which somehow disposes of the
     value stored under KEY, and <RegToken>=<ValueTokens> shall be a command
     which stores under KEY the value disposed of by the command sequence
     <ValueTokens>."
    (let* ((getter
            (lambda (match context ship)
              (declare (ignore ship))
;             (format *trace-output* "~&@@@ Getting from register ~A, passing to ~S~%"
;                     key *put-data-callback*)
              (put-data (lookup key register-table nil) context)
              (parser-expansion-state (token-source-state match) t)))
           (assign 
             (tokens% :chars "="))
           (setter
            (lambda (match context ship)
              (let* ((tsrc (token-source-state match))
                     (tok nil)
                     (stored nil)
                     (pstate (when (match-setf ((tok token*)) tsrc context)
                               (let ((*put-data-callback*
                                       (lambda (value context)
                                         (declare (ignore context))
;                                        (format *trace-output* "~&@@@ Setting register ~A to ~S~%"
;                                                key value)
                                         (setf stored t)
                                         (remember key register-table
                                                   value))))
                                 (mex-dispatch tok tsrc context ship)))))
                (if stored pstate
                    (parser-error-state* tsrc
                      "noData" (dispatching-token match) assign tok))))))
      (add-command key
        (make-builtin :pattern (vector) :handler getter)
        command-table *adjoin-strong*)
      (add-command key
        (make-builtin
          :pattern (vector (make-pattern-delimiter :tokens assign))
          :handler setter)
        command-table *adjoin-strong*)))

  (defbuiltin register (cmd-tok
                        :match match :context ctx :dispatching dispatching
                        :token-source tsrc)
    "Consume one command token, use it to obtain a command key, and create
     two builtins, used to store and retrieve a data object under that key
     from the register table of the native context of DISPATCHING (or its
     nearest opaque ancestor).  Record the builtins in the command table of
     the same context under the same key.  If the command token is followed
     by a `=', expand to the command key (allowing for initialization usages
     such as \\register\\foo=\\true).  Otherwise, expand to nothing."
    :pattern
    (match-setf-and-yield ((cmd-tok token #'command-token-p)) match ctx)
    :handler
    (let ((octx (ensure-opaque-context (token-context dispatching))))
      (if octx
          (bind/init ((cmd-table (command-table octx) (make-table))
                      (reg-table (register-table octx) (make-table)))
            (add-register-commands (token-command-key cmd-tok)
                                   cmd-table reg-table)
            (parser-expansion-state tsrc
              (or (not (token-is (next-token* tsrc ctx) :char #\=))
                  cmd-tok)))
          (parser-error-state* tsrc
            "noContextInput" dispatching cmd-tok))))


  ;;; Data constructors

  (defbuiltin true (:context ctx :token-source tsrc)
    "Dispose of the boolean true as a data object.  Consume nothing, and
     expand to nothing."
    (put-data t ctx)
    (parser-expansion-state tsrc t))

  (defbuiltin false (:context ctx :token-source tsrc)
    "Dispose of the boolean false as a data object.  Consume nothing, and
     expand to nothing."
    (put-data false ctx)
    (parser-expansion-state tsrc t))

  (defbuiltin token (tok :match match :context ctx :token-source tsrc)
    "Consume one (non-eot) token, subject to parameter expansion, and
     dispose of it as a data object.  Expand to nothing."
    :pattern
    (match-setf-and-yield ((tok token* #'non-eot-token-p)) match ctx)
    :handler
    (put-data tok ctx)
    (parser-expansion-state tsrc t))

  (defbuiltin tokens (:match match :context ctx :token-source tsrc)
    "Consume <Content>\\endtokens, where <Content> is any sequence of tokens
     not containing paragraph breaks.  Expand parameters in <Content>, and
     dispose of the sequence of tokens as a data object.  Expand to
     nothing."
    :pattern
    (loop for tok := (next-token/expand (token-source-state match) ctx)
          if (or (par-break-p tok) (eot-p tok))
            return nil
          else if (token-is tok :command "endtokens")
            do (setf (accumulator match) (ensure-vector content)
                     (terminator match) tok)
               (incf (match-length match))
               (incf (matched-token-count match))
            and return match
          else
            collect tok :into content
            and do (incf (match-length match)))
    :handler
    (put-data (accumulator match) ctx)
    (parser-expansion-state tsrc t))

  #| TBD: Correct handling of token sequences wrt. \pop etc. |#
  #| TBD: Check all code for cases where a token is expected, but an error
          display might sneak in. |#

  (defun match-int (match context)
    "Pattern matcher function for \\int etc.  Consume an integer
     representation, i. e. zero or more character tokens of category `other'
     followed by one or more character tokens of category `number', whose
     string representation can be converted to an integer.  Update match
     length and token source in MATCH, and store the integer value in the
     accumulator."
    (loop with tsrc := (token-source-state match)
          for tsrc- = tsrc
          for tok := (next-token/expand tsrc context)
          for length :from 1
          for cat := (and (char-token-p tok) (not (newline-token-p tok))
                          (ccat-base (token-category tok)))
          with some-digits := nil
          while (or (and (eql cat *ccat-number*) (setf some-digits t))
                    (unless some-digits (eql cat *ccat-other*)))
          collect (token-chr tok) :into str
          finally (let ((int (ensure-integer (ensure-string str))))
                    (return
                      (when (numberp int)
                        (setf (match-length match) length
                              (token-source-state match) tsrc-
                              (accumulator match) int)
                        match)))))

  (defbuiltin int (:match match :context ctx :token-source tsrc)
    "Consume an integer representation and dispose of the integer as data
     object.  Expand to nothing."
    :pattern
    (match-int match ctx)
    :handler
    (put-data (accumulator match) ctx)
    (parser-expansion-state tsrc t))

#|
@BEGIN TEST REGISTERS
@MEX
\register\foo=\false \register\bar=\int 42 \register\baz=\token\bar
\foo\pop ; \baz\pop\pop
\baz=\tokens quux}\endtokens
{\baz\pop
@JSON
{"t": " \nfalse; 42\n\nquux"},
@END TEST
|#


  ;;; Token comparison

  (defbuiltin is (tok1 tok2 :match match :context ctx :token-source tsrc)
    "Consume two (non-eot) tokens, subject to parameter expansion, and
     dispose of a boolean: true if the two tokens are the same (in the sense
     of TOKEN-EQUAL), false otherwise.  Expand to nothing."
    :pattern
    (match-setf-and-yield ((tok1 token* #'non-eot-p)
                           (tok2 token* #'non-eot-p))
      match ctx)
    :handler
    (put-data (token-equal tok1 tok2) ctx)
    (parser-expansion-state tsrc t))

  (defbuiltin iscommand (tok :match match :context ctx :token-source tsrc)
    "Consume one (non-eot) token, subject to parameter expansion, and
     dispose of a boolean: true if the token is a command token, false
     otherwise.  Expand to nothing."
    :pattern
    (match-setf-and-yield ((tok token* #'non-eot-p)) match ctx)
    :handler
    (put-data (command-token-p tok) ctx)
    (parser-expansion-state tsrc t))

  (defbuiltin ischr (tok :match match :context ctx :token-source tsrc)
    "Consume one (non-eot) token, subject to parameter expansion, and
     dispose of a boolean: true if the token is a character token, false
     otherwise.  Expand to nothing."
    :pattern
    (match-setf-and-yield ((tok token* #'non-eot-p)) match ctx)
    :handler
    (put-data (char-token-p tok) ctx)
    (parser-expansion-state tsrc t))

  (defbuiltin hascat (query-cat compare-base tok
                      :match match :context ctx :token-source tsrc)
    "Consume some tokens that constitute a character category specification,
     and one character token, subject to parameter expansion.  Dispose of a
     boolean: true if the character matches the specified category, false
     otherwise.  Expand to nothing."
    (declare (ignorable query-cat compare-base))
    :pattern
    (and (setf match (match-ccat-spec match ctx))
         (match-setf-and-yield ((tok token* #'char-token-p)) match ctx))
    :handler
    (let* ((arg-cat (token-category tok))
           (value (and (or (not compare-base)
                           (eql (ccat-base arg-cat)
                                (ccat-base query-cat)))
                       (or (not (ccat-constituent-p query-cat))
                           (ccat-constituent-p arg-cat))
                       (or (not (ccat-active-p query-cat))
                           (ccat-active-p arg-cat)))))
      (put-data value ctx)
      (parser-expansion-state tsrc t)))

#|
@BEGIN TEST TOKEN-COMPARISONS
@MEX
\edef\foo{\backslash foo}
\edef\bar{\backslash bar}
\def\test#\TOKi#\TOKii{#\TOKi\if\is#\TOKi#\TOKii\then=\else≠\endif#\TOKii}
\test\foo\foo; \test\foo\bar; \test12; \test22; \expandafter\test\chr\other11
\def\testcat#\TOK{<#\TOK> is \if\iscommand#\TOK\then\
  a command\elsif\ischr#\TOK\then\if\hascat\letter#\TOK\then\
  a letter\elsif\hascat\number#\TOK\then\
  a number digit\elsif\hascat\whitespace#\TOK\then\
  a whitespace\else\
  a special character\endif\
  \if\hascat+\constituent#\TOK\then{ }(constituent)\endif\endif}
\testcat{\foo}; \testcat{7}; \testcat{Ω}; \testcat{美};
\testcat{☺}; \testcat{ }; \testcat{ }
@JSON
{"t":"\n\n\n\\foo=\\foo; \\foo\u2260\\bar; 1\u22602; 2=2; 1\u22601\n\n<\\foo> is a command; <7> is a number digit; <\u03A9> is a letter (constituent); <\u7F8E> is a letter (constituent);\n<\u263A> is a special character; < > is a whitespace; <\u00A0> is a whitespace (constituent)"}
@END TEST
|#


  ;;; Operations on stacked arguments

  (defmacro if-stack-pop-bind ((&rest bindings) context seq &optional alt)
    "If data at the top of the DOM stack associated with CONTEXT matches
     BINDINGS by count and type, run SEQ in an environment where variables
     from BINDINGS are bound to the corresponding data objects.  If the data
     in stack doesn't match, and ALT is provided, run it instead.  In any
     case, data are popped off the stack prior to running SEQ or ALT.
       BINDINGS consist of a number of data binding specs followed by
     auxiliary bindings.  A data spec has the form (<VAR> <TYPE> [<PRED>]),
     where <VAR> is a symbol (variable name), <TYPE> is one of the literal
     symbols ANY, INT, BOOL, TOKEN, or ELEMENT, and <PRED> is an optional
     specifier for a predicate function, quasi defaulting to (CONSTANTLY T).
     A spec matches/binds a data object of the corresponding type, provided
     that it satisfies <PRED>.  Specs which are closer to the beginning of
     BINDINGS correspond to data deeper in the stack.
       Auxiliary bindings make up a property list which may include
     variable names indicated by the keywords :STACKS, :ELEMENTS, :CURRENT,
     :UNDERFLOW, and :CONTEXT.  The variable indicated by :STACKS is bound
     to the DOM multistack; the variable indicated by :ELEMENTS, to the
     element stack of the multistack; the variable indicated by :CURRENT, to
     the current stack; the variable indicated by :CONTEXT, to the CONTEXT
     (this is useful in DEFBUILTIN-STACK-FUNOPs); the variable indicated
     by :UNDERFLOW is bound to T if the stack doesn't have enough data to
     match other bindings, and to NIL otherwise."
    (let* ((aux-bindings (member-if #'keywordp bindings))
           (data-bindings (ldiff bindings aux-bindings)))
      (destructuring-bind (&key ((:stacks stacks-var) (ps-gensym "STACKS"))
                                ((:current current-var) (ps-gensym "CUR"))
                                ((:underflow underflow) (ps-gensym "UNDFL"))
                                ((:elements elements-var) (ps-gensym "ELTS")
                                 elements-var-p)
                                ((:context context-var) nil
                                 context-var-p)
                           &aux (underflow-check (ps-gensym "UNDFLCK")))
            aux-bindings
        (multiple-value-bind (data-pops data-checks)
            (loop for binding :in data-bindings
                  with pop and check
                  with pops and checks
                  do (destructure/or binding
                       ((var type &optional pred)
                        (and (symbolp var)
                             (let ((type-check
                                    (case type
                                      ((any) t)
                                      ((int) `(integerp ,var))
                                      ((bool) `(or (true-p ,var)
                                                   (false-p ,var)))
                                      ((token) `(token-p ,var))
                                      ((element) `(dom-element-p ,var))
                                      (t nil)))
                                   (pred-check
                                    (and pred `(funcall ,pred ,var))))
                               (when type-check
                                 (setf pop
                                         `(,var (and (not ,underflow)
                                                     (stack-pop
                                                       ,current-var
                                                       ,underflow-check)))
                                       check
                                         (if pred-check
                                             (if (eq type-check t)
                                                 pred-check
                                                 `(and ,type-check
                                                       ,pred-check))
                                            type-check)))))))
                   when pop
                     do (push pop pops) (push check checks)
                   else
                     do (error "Invalid IF-STACK-POP-BIND binding: ~S"
                               binding)
                   finally (return (values pops checks)))
          `(with-context-dom-stack (,stacks-var ,context
                                    :current ,current-var
                                    ,@(when elements-var-p
                                        `((:elements ,elements-var))))
             (let* ((,underflow nil)
                    (,underflow-check (lambda () (setf ,underflow t)))
                    ,@data-pops
                    ,@(when context-var-p `((,context-var ,context))))
               (declare (ignorable ,@(mapcar #'car data-pops)))
               (if (and (not ,underflow) ,@data-checks) ,seq ,alt)))))))

  (defun stack-op-error (dispatching underflow &rest args)
    "Return an error display for one of the two errors common to all stack
     operations: a stack underflow (e. g., a POP called with an empty stack)
     or an invalid argument (e. g., a DIV called on a non-integer or zero
     divider)."
    (declare (special *root-context*))
    (if underflow
        (error-display* "stackUnderflow" dispatching)
        (loop
          for arg :in args
          for arg-input
            := (cond
                 ((error-display-p arg)
                  (tokens% :chars (string #.(code-char #x2191))))
                 ((or (token-p arg) (group-p arg))
                  (vector-add
                   (tokens% :chars (string #.(code-char #xAB)))
                   (string-to-input (input-to-string arg)
                     *root-context* *plain-category-table*)
                   (tokens% :chars (string #.(code-char #xBB)))))
                 (t (data-to-input* arg *root-context*)))
          for args-input := arg-input
            :then (vector-add args-input (tokens% :chars " ") arg-input)
          for error := nil
            :then (if (error-display-p arg)
                      (error-display* :add-to error :append arg
                                      :sep ellipsis)
                      error)
          finally
            (return
              (error-display-add error
                :append-input dispatching :sep ellipsis
                :append-message
                  (vector-add
                    (tokens% :command "stackOpInvalidArgs" :chars "{")
                    args-input
                    (tokens% :chars "}")))))))

  (defbuiltin discard (:match match :context ctx :dispatching dispatching
                       :token-source tsrc)
    "Pop and discard one element off the stack.  Expand to nothing."
    (if-stack-pop-bind ((x any)) ctx
      (parser-expansion-state tsrc t)
      (parser-error-state tsrc (stack-op-error dispatching t))))

  (defbuiltin pop (:match match :context ctx :dispatching dispatching
                   :token-source tsrc)
    "Pop one element off the stack.  If disposing of the datum would not
     push it back on the stack (e. g., if it would be put into a register,
     or used to check a condition), do so and expand to nothing.  Otherwise,
     convert the datum to input and use that as expansion."
    (if-stack-pop-bind ((x any)) ctx
      (if *put-data-callback*
          (progn (funcall *put-data-callback* x ctx)
                 (parser-expansion-state tsrc t))
          (parser-expansion-state tsrc (data-to-input* x ctx)))
      (parser-error-state tsrc (stack-op-error dispatching t))))

  (defbuiltin bottom (:match match :context ctx :dispatching dispatching
                      :token-source tsrc)
    "Dispose of boolean true if the stack is empty, of false otherwise.
     Expand to nothing."
    (with-context-dom-stack (s ctx :current current)
      (put-data (stack-empty-p current) ctx)
      (parser-expansion-state tsrc t)))

#|
@BEGIN TEST STACK-OPS-1
@MEX
{\insignificantWhitespaces
  \int1 \int2 \int3
  \discard
  \parent\register\x=\pop
  \parent\register\y=\bottom
  \discard
  \parent\register\z=\bottom
  \pop
  \parent\register\err=\pop
}
\x\pop{} \y\pop{} \z\pop{}
\err\pop{}
@JSON
[{"t":"\n2 false true\n"},
 @ERROR IN "\\pop" ("Stack underflow")]
@END TEST
|#

  (defbuiltin dup (:match match :context ctx :dispatching dispatching
                   :token-source tsrc)
    "Duplicate the top element of the stack.  Expand to nothing."
    (if-stack-pop-bind ((x any) :current current) ctx
      (progn (stack-push x current) (stack-push x current)
             (parser-expansion-state tsrc t))
      (parser-error-state tsrc (stack-op-error dispatching t))))

  (defbuiltin exch (:match match :context ctx :dispatching dispatching
                    :token-source tsrc)
    "Exchange the two top elements of the stack.  Expand to nothing."
    (if-stack-pop-bind ((x any) (y any) :current current) ctx
      (progn (stack-push y current) (stack-push x current)
             (parser-expansion-state tsrc t))
      (parser-error-state tsrc (stack-op-error dispatching t))))

  (defbuiltin roll (:match match :context ctx :dispatching dispatching
                    :token-source tsrc)
    "Pop a non-negative integer N and an integer P off the stack, which
     should then contain at least N elements.  Rotate by P positions the top
     N elements of the stack (see STACK-ROTATE).  Expand to nothing."
    (if-stack-pop-bind ((scope int (lambda (x) (>= x 0))) (value int)
                        :current current :underflow underflow)
        ctx
      (progn
        (stack-rotate current scope value (lambda () (setf underflow t)))
        (if underflow
            (parser-error-state tsrc (stack-op-error dispatching t))
            (parser-expansion-state tsrc t)))
      (parser-error-state tsrc
        (stack-op-error dispatching underflow scope value))))

#|
@BEGIN TEST STACK-OPS-2
@MEX
ABCDE\exch\dup \int5\int-3\roll \int4\int2\roll 42\roll\exch
\int12\int3\roll
\int-2\int5\roll
@JSON
[{"t":"ADCED"},
 @ERROR IN "\\roll" ("Data on stack are not valid arguments for this operation: «4» «2»"),
 {"t":"B\n"},
 @ERROR IN "\\roll" ("Stack underflow"),
 {"t":"\n"},
 @ERROR IN "\\roll" ("Data on stack are not valid arguments for this operation: -2 5")] 
@END TEST
|#

  (defmacro defbuiltin-stack-funop (name operands expr)
    "Define a builtin NAME which pops as many elements of specified types
     off the stack as there are OPERANDS, computes a function of them (given
     by EXPR), and disposes of the value."
    (with-ps-gensyms (match dispatching context tsrc underflow)
      `(defbuiltin ,name (:match ,match :context ,context
                          :dispatching ,dispatching :token-source ,tsrc)
         (if-stack-pop-bind (,@operands :underflow ,underflow) ,context
           (progn (put-data ,expr ,context)
                  (parser-expansion-state ,tsrc t))
           (parser-error-state ,tsrc
             (stack-op-error ,dispatching ,underflow
                             ,@(loop for op :in operands
                                     until (keywordp op)
                                     collect (car op))))))))

  ;; Type checking

  (defbuiltin-stack-funop haveint ((x any))
    (integerp x))

  (defbuiltin-stack-funop havebool ((x any))
    (or (true-p x) (false-p x)))

  (defbuiltin-stack-funop havetoken ((x any))
    (token-p x))

  (defbuiltin-stack-funop haveerror ((x any))
    (error-display-p x))

  (defbuiltin-stack-funop haveelement ((x any))
    (dom-element-p x))

  (defbuiltin-stack-funop havecomment ((x any))
    (dom-comment-p x))

  (defbuiltin-stack-funop haverecipe ((x any))
    (dom-recipe-p x))

  ;; Arithmetic ops

  (defbuiltin-stack-funop add ((x int) (y int))
    (+ x y))

  (defbuiltin-stack-funop sub ((x int) (y int))
    (- x y))

  (defbuiltin-stack-funop mul ((x int) (y int))
    (* x y))

  (defbuiltin-stack-funop div ((x int) (y int (lambda (y) (/= y 0))))
    (truncate x y))

  (defbuiltin-stack-funop rem ((x int) (y int (lambda (y) (/= y 0))))
    (rem x y))

  (defbuiltin-stack-funop gt ((x int) (y int))
    (> x y))

  (defbuiltin-stack-funop geq ((x int) (y int))
    (>= x y))

  (defbuiltin-stack-funop lt ((x int) (y int))
    (< x y))

  (defbuiltin-stack-funop leq ((x int) (y int))
    (<= x y))

  ;; Logic ops

  (defbuiltin-stack-funop conj ((x bool) (y bool))
    (and x y))

  (defbuiltin-stack-funop disj ((x bool) (y bool))
    (or x y))

  (defbuiltin-stack-funop neg ((x bool))
    (not x))

  (defbuiltin-stack-funop eq ((x any) (y any))
    (or (eq x y)
        (and (numberp x) (numberp y) (= x y))
        (and (false-p x) (false-p y))
        (token-equal x y)))

  ;; Char ops

  (defbuiltin-stack-funop charint ((x token #'char-token-p))
    (char-code (token-chr x)))

  (defbuiltin-stack-funop intchar ((x int
                                    (lambda (x)
                                      (<= #.(min-char *category-table*)
                                          x
                                          #.(max-char *category-table*))))
                                   :context ctx)
    (guarded-make-char-token :start 0 :end 1 :context ctx
                             :chr (code-char x)
                             :category (char-cat x (category-table ctx))))

  (defbuiltin-stack-funop upcase ((x token #'char-token-p))
    (let ((ux (copy-structure x)))
      (setf (token-chr ux) (upcase (token-chr x)))
      ux))

  (defbuiltin-stack-funop downcase ((x token #'char-token-p))
    (let ((ux (copy-structure x)))
      (setf (token-chr ux) (downcase (token-chr x)))
      ux))

  ;; DOM ops

  (defbuiltin open (elt-name
                    :match match :context ctx :dispatching dispatching
                    :token-source tsrc)
    :pattern
    (match-setf-and-yield ((elt-name group #'group-p)) match ctx)
    :handler
    (parser-state-bind (:accumulator elt-name-expn :error expn-errors)
        (get-full-expansion elt-name tsrc ctx t)
      (if expn-errors
          (parser-error-state tsrc
            (error-display* :add-to expn-errors :prepend-input dispatching
                            :sep ellipsis))
          (with-context-dom-stack (stacks ctx)
            (dom-stack-open-element
              (element (input-to-string elt-name-expn))
              stacks)
            (parser-expansion-state tsrc t)))))

  (defbuiltin close (:match match :context ctx)
    :pattern
    (when (match-setf-update ((:command "current")) match ctx)
      match)
    :handler
    (with-context-dom-stack (stacks ctx)
      (dom-stack-close-element stacks))
    (parser-expansion-state (token-source-state match) t))

  #| TBD: \close{<Elt>}, \close\block, etc.
     TBD: DOM lookup + \reopen |#

)

;;; Local Variables: ***
;;; mode:lisp ***
;;; local-font-lock-keywords:(("(\\(def\\(?:\\(ps\\(?:macro\\|fun\\)\\|\\(?:un\\|macro\\)\\+ps\\|builtin\\(?:-iterator\\)?\\)\\|\\(guard\\|enum\\)\\|\\(struct-guarded\\)\\)\\)[ \t\n]+(?\\([^()]*?\\)[ \t\n]" (1 font-lock-keyword-face) (5 (cond ((match-beginning 2) font-lock-function-name-face) ((match-beginning 3) font-lock-variable-name-face) ((match-beginning 4) font-lock-type-face)))) ("(\\(ambi-ps\\)\\_>" (1 font-lock-preprocessor-face))) ***
;;; End: ***
