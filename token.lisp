(in-package #:mex)

;;; Error displays

(ambi-ps ()

  (defstruct (error-display)
    faulty-input message)

  (defun error-display-add (orig
                            &key prepend-input append-input replace-input
                                 prepend-message append-message)
    "If ORIG is an error display, prepend or append the given input and
     message, as specified.  Otherwise, create a new error display with the
     given input, message and SHIFT-CONTEXT."
    (make-error-display
      :faulty-input (let ((fi (if (error-display-p orig)
                                  (error-display-faulty-input orig)
                                  (ensure-vector orig))))
                      (cond
                        (prepend-input
                         (vector-add (ensure-vector prepend-input) fi))
                        (append-input
                         (vector-add fi (ensure-vector append-input)))
                        (replace-input
                         (ensure-vector replace-input))
                        (t fi)))
      :message (if (error-display-p orig)
                   (let ((msg (ensure-vector (error-display-message orig))))
                     (cond
                       (prepend-message
                        (spliced msg 0 0
                                 (ensure-vector prepend-message)))
                       (append-message
                        (spliced msg (length msg) 0
                                 (ensure-vector append-message)))
                       (t msg)))
                   (let ((msg (or prepend-message append-message)))
                     (and msg (vector (ensure-vector msg)))))))

  (defmacro error-display* (&rest args)
    "Wrapper for using MAKE-ERROR-DISPLAY and ERROR-DISPLAY-ADD with error
     message name macros."
    (labels ((arg-vector (args &optional (fn 'ensure-vector))
               (if (null (cdr args))
                   `(,fn ,@args)
                   `(vector-add ,@(mapcar (lambda (arg) `(,fn ,arg))
                                          args))))
             (opt (args kwd kwd2 msg-p)
               (let ((arg (getf args kwd))
                     (arg2 (and (keywordp kwd2) (getf args kwd2))))
                 (when (or arg arg2)
                   (let* ((val0 (and arg
                                     (if msg-p
                                         (tokens** :command (car arg))
                                         (arg-vector (reverse arg)))))
                          (val2 (if arg2
                                    (arg-vector
                                      (reverse arg2)
                                      (if msg-p
                                          'error-display-message
                                          'error-display-faulty-input))))
                          (val (case (and val0 val2 kwd2)
                                 ((:prepend) `(vector-add ,val2 ,val0))
                                 ((:append) `(vector-add ,val0 ,val2))
                                 (t (or val0 val2)))))
                     (list kwd val))))))
      (if (keywordp (car args))
          (loop for arg :in args
                with current-kwd := nil
                with args* := '()
                if (keywordp arg)
                  do (setf current-kwd arg)
                else
                  do (push arg (getf args* current-kwd))
                finally
                   (return
                     `(error-display-add ,(car (getf args* :add-to))
                        ,@(opt args* :prepend-message :prepend t)
                        ,@(opt args* :append-message :append t)
                        ,@(opt args* :prepend-input :prepend nil)
                        ,@(opt args* :append-input :append nil)
                        ,@(opt args* :replace-input nil nil))))
          `(make-error-display
             :message (vector ,(tokens** :command (car args)))
             :faulty-input ,(arg-vector (cdr args))))))

  (defun faulty-input-first (error-display)
    "Return the first token or group of the faulty input displayed by
     ERROR-DISPLAY."
    (aref* (error-display-faulty-input error-display) 0))

  (defun faulty-input-last (error-display)
    "Return the last token or group of the faulty input displayed by
     ERROR-DISPLAY."
    (aref* (error-display-faulty-input error-display) -1))

)

;;; Tokens

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; A token represents one of the following substrings of a source text:
  ;;                 <token> ::= <param-token> | <basic-token>
  ;;           <basic-token> ::= <char-token> | <newline-token> | <cmd-token>
  ;;           <param-token> ::= <param-escape> <basic-token>
  ;;          <param-escape> ::= any character with base category param;
  ;;             <cmd-token> ::= <escape> <cmd-name> <whitespace>
  ;;                           | <escape> <cmd-name>
  ;;                <escape> ::= any character with base category escape;
  ;;              <cmd-name> ::= <noncon> | <con-seq>
  ;;               <con-seq> ::= <con-seq> <con> | <con>
  ;;                   <con> ::= any character with constituent flag on
  ;;                             and non-newline base category;
  ;;                <noncon> ::= any character which is not <con>;
  ;;         <newline-token> ::= <newline-noncon> | <newline-con-seq>
  ;;        <newline-noncon> ::= any character with base category newline
  ;;                             and constituent flag off;
  ;;       <newline-con-seq> ::= two different characters with base category
  ;;                             newline and constituent flag on;
  ;;            <char-token> ::= <bare-char> | <param-escape-literal>
  ;;  <param-escape-literal> ::= <param-escape> <param-escape>
  ;;             <bare-char> ::= any character with base category other than
  ;;                             escape, param, or newline.
  ;; E. g., according to the standard category table, the following are
  ;; <char-token>s:
  ;;        a 9 @     <bare-char>s of categories letter, number, other;
  ;;      TAB { }     <bare-char>s of categories whitespace, lbrace, rbrace;
  ;;           ##     a <param-escape-literal>.
  ;; Examples of other kinds of tokens:
  ;;        CR+LF     a <newline-token> with <newline-con-seq>.
  ;;           \*     a <cmd-token> with <noncon>;
  ;;         \par     a <cmd-token> with <con-seq>;
  ;;     #1 #x #+     <param-token>s over <char-token>s;
  ;;        #\abc     a <param-token> over <cmd-token>.

  (ambi-ps ()

    (defguard token 1000000)

    (defstruct-guarded (token (:guard token))
      start end context)

    (defstruct-guarded (char-token (:include token)
                                   (:conc-name token-)
                                   (:guard token))
      chr category)

    (defstruct-guarded (newline-token (:include char-token)
                                      (:conc-name token-)
                                      (:guard token))
      chr2 par-end)

    (defstruct-guarded (command-token (:include token)
                                      (:conc-name token-)
                                      (:guard token))
      name escape-chr)

    (defstruct-guarded (param-token (:include token)
                                    (:conc-name token-)
                                    (:guard token))
      arg-token param-escape-chr)

    (defstruct-guarded (eot-token (:include token) (:guard token)))

  )

  (defun token* (type src)
    (flet ((make-dispatch-token (name)
             (etypecase name
               (character
                 (make-char-token
                   :chr name
                   :category (logior *ccat-active* (char-cat name))))
               (string
                 (make-command-token
                   :escape-chr #\\ :name name)))))
      (let ((tok
             (case type
               ((:char)
                (make-char-token :chr src :category (char-cat src)))
               ((:command)
                (make-dispatch-token src))
               ((:param)
                (make-param-token
                  :param-escape-chr #\#
                  :arg-token (make-dispatch-token src)))
               (t (error "Invalid TOKEN* type: ~S" type)))))
        (setf (token-start tok) 0 (token-end tok) 1)
        tok)))

  (defun token** (type src)
    "Wrapper for TOKEN* to allow its use in ParenScript macros."
    `(struct (lisp (token* ,type ,src))))

  (defun tokens* (&rest input)
    "Return a vector of tokens specified by arguments in INPUT which should
     have the format <INPUT> ::= {(<INPUT>) | <TYPE> <SRC>}*.  A <TYPE> of
     :CHARS with a string <SRC> produces a sequence of character tokens; a
     <TYPE> of :COMMAND with a string <SRC> produces one command token with
     the name <SRC>, whereas with a char <SRC> it produces an active
     character token; a <TYPE> of :PARAM produces one parameter token over a
     command or active character token; and a list (<INPUT>) produces tokens
     framed by left and right brace tokens."
    (let ((tokens (make-stack)) (i 0))
      (labels ((push-token (tok)
                 (setf (token-start tok) i
                       (token-end tok) (incf i))
                 (stack-push tok tokens))
               (push-tokens (input)
                 (loop
                   until (endp input)
                   if (listp (car input))
                     do (push-token
                          (make-char-token :chr #\{ :category *ccat-lbrace*))
                        (push-tokens (pop input))
                        (push-token
                          (make-char-token :chr #\} :category *ccat-rbrace*))
                   else
                     do (destructuring-bind (type src &rest input+) input
                          (case type
                            ((:chars)
                             (loop
                               for c :across src
                               do (push-token (token* :char c))))
                            ((:command :param)
                             (push-token (token* type src)))
                            (t (error "Invalid TOKENS* type: ~S" type)))
                          (setf input input+)))))
        (push-tokens input)
        (ensure-vector tokens))))

  (defun tokens** (&rest spec)
    "Wrapper for TOKENS* to allow its use in ParenScript macros."
    `(struct (lisp (tokens* ,@spec))))

  (defmacro+ps tokens (&rest spec)
    "Notation for token vector literals; see TOKENS*."
    (apply #'tokens** spec))

)

(ambi-ps ()

  (defun lbrace-token-p (thing)
    "Return true iff THING is a character token with category non-active
     left brace."
    (and (char-token-p thing)
         (let ((cat (token-category thing)))
           (and (eq (ccat-base cat) *ccat-lbrace*)
                (not (ccat-active-p cat))))))

  (defun rbrace-token-p (thing)
    "Return true iff THING is a character token with category non-active
     right brace."
    (and (char-token-p thing)
         (let ((cat (token-category thing)))
           (and (eq (ccat-base cat) *ccat-rbrace*)
                (not (ccat-active-p cat))))))

  (defun active-token-p (thing)
    "Return true iff THING is a charcter token with an active category."
    (and (char-token-p thing)
         (ccat-active-p (token-category thing))))

  (defun dispatching-token-p (thing)
    "Return true iff THING is a token which may be expanded."
    (or (command-token-p thing) (active-token-p thing)))

  (defgeneric eot-p (thing)
    (:documentation "Return true iff THING signifies an end of text.")
    (:method (thing) nil)
    (:method ((tok eot-token)) t)
    (:method ((ed error-display)) (eot-p (faulty-input-last ed))))

  (defgeneric par-break-p (thing)
    (:documentation "Return true iff THING signifies a paragraph break.")
    (:method (thing) nil)
    (:method ((tok newline-token)) (token-par-end tok)))

  (defgeneric input-start (thing)
    (:documentation "Return the character position where THING starts.")
    (:method (thing) nil)
    (:method ((tok token)) (token-start tok))
    (:method ((ed error-display)) (input-start (faulty-input-first ed))))

  (defgeneric input-end (thing)
    (:documentation "Return the character position where THING ends.")
    (:method (thing) nil)
    (:method ((tok token)) (token-end tok))
    (:method ((tok newline-token)) (or (token-par-end tok) (token-end tok)))
    (:method ((ed error-display)) (input-end (faulty-input-last ed))))

  (declaim
    (ftype function
      char-token-at newline-token-at command-token-at param-token-at))

  (defun token-at (source position context)
    "Return the token which can be parsed out from the string SOURCE at
     POSITION, assuming the character categories in CONTEXT.  If EXPAND is
     true, store the token in the token cache."
    (if (or (not source) (>= position (length source)))
        (guarded-make-eot-token :start position :end position)
        (let* ((c (char-at source position))
               (cat (char-cat c)))
          (case (ccat-base cat)
            ;; Invalid characters are skipped.
            (#.*ccat-invalid*
             (token-at source (1+ position) context))
            (#.*ccat-newline*
             (newline-token-at source position context c cat))
            ;; The following two functions may return error displays
            ;; instead of tokens.
            (#.*ccat-escape*
             (command-token-at source position context c))
            (#.*ccat-param*
             (param-token-at source position context c))
            (t ; (*ccat-lbrace* *ccat-rbrace* *ccat-whitespace*
               ;  *ccat-letter* *ccat-number* *ccat-other*)
             (char-token-at source position context c cat))))))

  (defun skip-empty-lines (source start cont)
    "If the string SOURCE has an empty line at START, call CONT on the
     position of the first character of the first non-empty line in the
     string SOURCE after START.  Otherwise, call CONT with no arguments.  (A
     line is terminated by a newline-category character; it is empty if it
     contains only blank and / or invalid characters.)"
    (loop for i :from start :below (length source)
          for cat := (ccat-base (char-cat (char-at source i)))
          with last-newline := nil
          while (or (eq cat *ccat-whitespace*) (eq cat *ccat-newline*)
                    (eq cat *ccat-invalid*))
          when (eq cat *ccat-newline*) do (setf last-newline i)
          finally (return (cond
                            (last-newline (funcall cont (1+ last-newline)))
                            ((not cat) (funcall cont start))
                            (t (funcall cont))))))

  (defun newline-token-at (source position context c cat)
    (declare (ignore context))
    "Parse SOURCE at POSITION for a newline token."
    (let* ((position+ (1+ position))
           (c+ (and (< position+ (length source))
                    (char-at source position+))))
      (flet ((token (tok-start tok-end chr &optional chr2)
               (skip-empty-lines source tok-start
                 (lambda (&optional past-empty-lines)
                   (guarded-make-newline-token
                     :start tok-start :end tok-end :context context
                     :category cat :chr chr :chr2 chr2
                     ;; Empty lines generally denote paragraph breaks
                     :par-end past-empty-lines)))))
        (if (and c+
                 (ccat-constituent-p cat *ccat-newline*)
                 (ccat-constituent-p (char-cat c+) *ccat-newline*)
                 (not (eql c c+)))
            (token position (1+ position+) c c+)
            (token position position+ c)))))

  (defun skip-whitespace (source start cont)
    "If the character in the string SOURCE at START is a whitespace, call
     CONT on the position after it.  If it is an escape character followed
     by a newline token, call CONT on the position of the first
     non-whitespace after the newline.  Otherwise, call CONT on START."
    (let ((cat (char-cat (char-at source start))))
      (cond ((not (numberp cat))
             (funcall cont start))
            ((eq (ccat-base cat) *ccat-whitespace*)
             (funcall cont (1+ start)))
            ((eq (ccat-base cat) *ccat-escape*)
             (let* ((start+ (1+ start))
                    (cat+ (char-cat (char-at source start+))))
               (if (and (numberp cat+)
                        (eq (ccat-base cat+) *ccat-newline*))
                   (let* ((start++ (1+ start+))
                          (cat++ (char-cat (char-at source start++)))
                          (startln (if (and (ccat-constituent-p cat+)
                                            (numberp cat++)
                                            (ccat-constituent-p
                                              cat++ *ccat-newline*))
                                       (1+ start++)
                                       start++)))
                     (loop for i :from startln :below (length source)
                           for cat := (char-cat (char-at source i))
                           while (eq (ccat-base cat) *ccat-whitespace*)
                           finally (return (funcall cont i))))
                   (funcall cont start))))
            (t (funcall cont start)))))

  (defun skip-constituent (source start cont)
    "Call CONT on the position of the first character in the string SOURCE
     after START which is not a constituent character.  If the sequence of
     constituents is followed by a whitespace, CONT is passed a second
     argument, the position of the first character after the whitespace."
    (loop for i :from start :below (length source)
          for cat := (char-cat (char-at source i))
          while (ccat-constituent-p cat (- *ccat-newline*))
          finally (return
                    (if (numberp cat)
                        (skip-whitespace source i
                          (lambda (past-whitespace)
                            (if (= past-whitespace i)
                                (funcall cont i)
                                (funcall cont i past-whitespace))))
                        (funcall cont i)))))

  (defun command-token-at (source position context c)
    "Parse SOURCE at POSITION for a newline token."    
    (let ((position+ (1+ position)))
      (if (< position+ (length source))
          (let* ((c+ (char-at source position+))
                 (cat+ (char-cat c+)))
            (if (eq (ccat-base cat+) *ccat-newline*)
                (skip-whitespace source position
                  (lambda (past-whitespace)
                    (guarded-make-char-token
                      :start position :end past-whitespace :context context
                      :chr (char-at source (1- past-whitespace))
                      :category *ccat-whitespace*)))
                (let* ((name-end (1+ position+))
                       (tok-end name-end))
                  (if (ccat-constituent-p cat+ (- *ccat-newline*))
                      (skip-constituent source name-end
                        (lambda (past-consts &optional past-whitespace)
                          (setf name-end past-consts
                                tok-end (if (null past-whitespace)
                                            past-consts
                                            past-whitespace))))
                      (skip-whitespace source name-end
                         (lambda (past-whitespace)
                           (setf tok-end past-whitespace))))
                  (guarded-make-command-token
                    :start position :end tok-end
                    :name (substring source position+ name-end)
                    :escape-chr c :context context))))
          (error-display* "eotInCommandToken"
            (make-command-token
              :start position :end position+ :context context
              :escape-chr c :name "")
            (make-eot-token :start position+ :end position+)))))

  (defun param-token-at (source position context c)
    "Parse SOURCE at POSITION for a parameter token."
    (flet ((display-error (eot)
             (error-display*
               :add-to eot
               :prepend-input (make-param-token
                                :start position :end (1+ position)
                                :context context :param-escape-chr c
                                :arg-token (make-token))
               :append-message "eotInParamToken")))
      (let ((position+ (1+ position)))
        (if (< position+ (length source))
            (let* ((c+ (char-at source position+))
                   (cat+ (char-cat c+)))
              (if (eq (ccat-base cat+) *ccat-param*)
                  (guarded-make-char-token
                    :start position :end (1+ position+) :context context
                    :chr c+ :category cat+)
                  (let ((tok+ (token-at source position+ context)))
                    (if (eot-p tok+)
                        (display-error tok+)
                        (guarded-make-param-token
                          :start position :end (token-end tok+)
                          :arg-token tok+ :param-escape-chr c
                          :context context)))))
            (display-error
              (make-eot-token :start position+ :end position+))))))

  (defun char-token-at (source position context c cat)
    (declare (ignore source))
    "Parse (really not) SOURCE at POSITION for a character token."   
    (guarded-make-char-token :start position :end (1+ position)
                             :category cat :chr c :context context))

  (defun token-equal (a b)
    "Return true iff A and B are equal tokens."
    (or (and (newline-token-p a) (newline-token-p b))
        (and (char-token-p a) (char-token-p b)
             (or (not (token-chr a)) (not (token-chr b))
                 (equal (token-chr a) (token-chr b)))
             (eq (token-category a) (token-category b)))
        (and (command-token-p a) (command-token-p b)
             (equal (token-name a) (token-name b)))
        (and (param-token-p a) (param-token-p b)
             (token-equal (token-arg-token a) (token-arg-token b)))
        (and (eot-token-p a) (eot-token-p b))))

  (defmacro token-is (a b-type b-src)
    `(token-equal ,a ,(token** b-type b-src)))

  (defun token-context-equal (a b)
    "Return true iff A and B are equal tokens located at the same position
     and have the same context."
    (and (token-equal a b)
         (eq (token-context a) (token-context b))
         (eq (token-start a) (token-start b))
         (eq (token-end a) (token-end b))))

  (defun token-type-equal (a b)
    "Return true iff A and B are tokens of the same kind."
    (or (and (newline-token-p a) (newline-token-p b))
        (and (char-token-p a) (char-token-p b)
             (eq (token-category a) (token-category b)))
        (and (command-token-p a) (command-token-p b))
        (and (param-token-p a) (param-token-p b))
        (and (eot-token-p a) (eot-token-p b))))

  (defgeneric input-string (input)
    (:documentation "Return the string representation of an input element.")
    (:method (input)
      (if (vectorp input)
	  (loop for part :across input
	        for str := (input-string part)
	                :then (concatenate 'string str (input-string part))
	        finally (return str))
	  ""))
    (:method ((token char-token))
      (string (token-chr token)))
    (:method ((token newline-token))
      (if (token-chr2 token)
          (interpolate "#(token-chr token)#(token-chr2 token)")
          (string (token-chr token))))
    (:method ((token command-token))
      (if (token-escape-chr token)
          (interpolate "#(token-escape-chr token)#(token-name token)")
          (string (token-name token))))
    (:method ((token param-token))
      (let ((arg-str (input-string (token-arg-token token))))
        (interpolate "#(token-param-escape-chr token)#{arg-str}"))))

)

(defgeneric token-format (token)
  (:documentation
   "Format for printing token details in the REPL, in traces, etc.")
  (:method ((token token))
    nil)
  (:method ((token char-token))
    (list "~S of ~S" (token-chr token) (token-category token)))
  (:method ((token newline-token))
    (let ((par-end (token-par-end token)))
      (and par-end (list "PAR-END=~S" par-end))))
  (:method ((token command-token))
    (token-short-format token))
  (:method ((token param-token))
    (token-short-format token)))

(defun token-short-format (token)
  "Format for printing tokens as parts of larger structures in the REPL, in
   traces, etc."
  (let ((str (input-string token)))
    (list "~A" (substitute-if #\Space (property-test "Cc") str))))

(defmethod print-object ((token token) stream)
  (if *print-readably*
      (error 'print-not-readable :object token)
      (print-unreadable-object (token stream :type t)
        (apply #'format stream "[~S, ~S)~^~@[ :: ~@?~]"
               (token-start token) (token-end token)
               (token-format token)))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun command-key (s)
    "Take the name of a command and return the string for looking up the
     command definition in a table."
    (format nil "\\~A" (if (stringp s) s (symbol-to-js-string s))))

  (defpsfun command-key (s)
    "Take the name of a command and return the string for looking up the
     command definition in a table."
    (interpolate "\\#s"))

)

(ambi-ps ()

  (defgeneric token-command-key (token)
    (:documentation
     "Return the key to look up commands with functor TOKEN in a table.")
    (:method ((token command-token))
      (command-key (token-name token)))
    (:method ((token param-token))
      (token-command-key (token-arg-token token)))
    (:method ((token char-token))
      (string (token-chr token)))
    (:method (any) ""))

)

;;; Token sources

(ambi-ps ()

  (defstruct (token-source (:conc-name))
    "A structure to keep track of tokens read and about to be read.
     In a primary source, CHAR-SOURCE is the string with Mex text to
     process, CHAR-SOURCE-OFFSET is the position in the string at which the
     next token is to be read (as by TOKEN-AT), and CACHED-CAT-TABLE is the
     category table which was used to parse the last token read.
     CACHED-TOKENS is a stack/array which serves a double purpose: in a
     primary source, it caches read tokens; otherwise, it carries tokens
     resulting from command or alias expansion.  CACHED-TOKEN-OFFSET is the
     position at which tokens in a primary source are inserted into the
     cache, and at which tokens in an expansion source are to be retrieved.
     PARENT-SOURCE is the token source which should be used after the
     current source runs out of tokens.  EXPANSION-CONTEXT is a context for
     macro parameters (cf. function MEX-DISPATCH in command.lisp)."
    char-source char-source-offset cached-tokens cached-token-offset
    cached-cat-table parent-source expansion-context)

  (defun alias-expansion (token context)
    "Scan CONTEXT and its parents (bottom-up) for a definition of alias with
     functor TOKEN.  If there is such an alias, return its expansion tokens
     nativized in CONTEXT."
    (loop for ctx := context :then (parent-context ctx)
          while ctx
          with key := (token-command-key token)
          for expn := (and (alias-table ctx)
                           (lookup key (alias-table ctx)))
          if expn
            return (and (> (length expn) 0)
                        (loop for tok :across expn
                              for tok* := (copy-structure tok)
                              do (setf (token-context tok*) context)
                              collect tok* :into expn*
                              finally (return (ensure-vector expn*))))))

  (defun special-command-p (token &optional name)
    "Return true iff TOKEN is the functor of a special command (any one, or
     the one with the given NAME)."
    (flet ((specialp (nm)
             (or (equal nm "noexpand")
                 (equal nm "par")
                 #| ... |#)))
      (and (command-token-p token)
           (if name
               (and (equal (token-name token) name)
                    (specialp name))
               (specialp (token-name token))))))

  (declaim (ftype function simulate-command-with-input))

  (defun next-token (token-source context &optional (expand-aliases t))
    "Extract and return next token from TOKEN-SOURCE, given the character
     category and alias definitions in CONTEXT (however, aliases are not
     taken into account if the flag EXPAND-ALIASES is false)."
    (if token-source
        (let ((*category-table*
               (category-table (ensure-opaque-context context))))
          (cond
            ((not (cached-tokens token-source))
             (setf (cached-tokens token-source) (make-stack)
                   (cached-token-offset token-source) 0))
            #| TBD: compare with the stored token's context's cat-table
               instead. |#
            ((and (not (eq (cached-cat-table token-source) *category-table*))
                  (char-source token-source))
             (stack-clear (cached-tokens token-source)
                          (- (length (cached-tokens token-source))
                             (cached-token-offset token-source)))))
          (let* ((tok
                  (if (< (cached-token-offset token-source)
                         (length (cached-tokens token-source)))
                      (aref (cached-tokens token-source)
                            (cached-token-offset token-source))
                      (let ((tok (token-at
                                   (char-source token-source)
                                   (or (char-source-offset token-source) 0)
                                   context)))
                        (when (stack-p (cached-tokens token-source))
                          (unless (eot-p tok)
                            (stack-push tok (cached-tokens token-source)))
                          (setf (cached-cat-table token-source)
                                *category-table*))
                        tok)))
                 (alias-expn
                  (when (and expand-aliases
                             (dispatching-token-p tok)
                             (not (special-command-p tok)))
                    (alias-expansion tok context))))
            (when (newline-token-p tok)
              (simulate-command-with-input "par" nil context nil))
            (if alias-expn
                (let ((tsrc+ (source-after-next-token
                               token-source context tok)))
                  (setf (cached-tokens token-source) alias-expn
                        (cached-token-offset token-source) 0
                        (char-source token-source) nil
                        (char-source-offset token-source) nil
                        (parent-source token-source) tsrc+
                        (expansion-context token-source) nil)
                  (aref alias-expn 0))
                tok)))
        (make-eot-token)))

  (defun source-after-next-token (token-source
                                  &optional context
                                            (token (next-token
                                                     token-source context)))
    "Return a copy of TOKEN-SOURCE with offset(s) shifted to just after
     TOKEN."
    (if (eot-p token)
        (and token-source (parent-source token-source))
        (make-token-source
          :char-source (char-source token-source)
          :char-source-offset (and (char-source token-source)
                                   (input-end token))
          :cached-tokens (cached-tokens token-source)
          :cached-token-offset (1+ (cached-token-offset token-source))
          :cached-cat-table (cached-cat-table token-source)
          :parent-source (parent-source token-source)
          :expansion-context (expansion-context token-source))))

  (defmacro next-token/shift (token-source-location context
                              &optional (expand-aliases t))
    "Extract and return next token from the source at TOKEN-SOURCE-LOCATION,
     at which then store a copy of the source with offset(s) shifted to just
     after that token."
    (with-ps-gensyms (tok)
      `(loop for ,tok := (next-token ,token-source-location ,context
                                     ,expand-aliases)
             do (setf ,token-source-location
                        (source-after-next-token ,token-source-location
                                                 ,context
                                                 ,tok))
             if (not (and (eot-p ,tok) ,token-source-location))
               return ,tok)))

  (defun next-char (token-source context)
    "Extract and return just one character from TOKEN-SOURCE."
    (or (and (char-source token-source)
             (char-source-offset token-source)
             (< (char-source-offset token-source)
                (length (char-source token-source)))
             (char-at (char-source token-source)
                      (char-source-offset token-source)))
        (let ((tok (next-token token-source context nil)))
          (if (eot-p tok)
              (let ((tsrc+ (parent-source token-source)))
                (and tsrc+ (next-char tsrc+ context)))
              (and (token-p tok)
                   (let ((str (input-string tok)))
                     (and (> (length str) 0)
                          (char-at str 0))))))))

  (defun source-after-next-char (token-source context)
    "Return a copy of TOKEN-SOURCE with offset(s) shifted by one character."
    (if (and (char-source token-source)
             (char-source-offset token-source)
             (< (1+ (char-source-offset token-source))
                (length (char-source token-source))))
        (make-token-source
          :char-source (char-source token-source)
          :char-source-offset (1+ (char-source-offset token-source))
          :cached-tokens (cached-tokens token-source)
          :cached-token-offset (cached-token-offset token-source)
          :parent-source (parent-source token-source)
          :expansion-context (expansion-context token-source))
        (let* ((tok (next-token token-source context nil))
               (str (and (token-p tok) (input-string tok))))
          (if (and str (> (length str) 0))
              (make-token-source
                :char-source str
                :char-source-offset 1
                :parent-source (source-after-next-token
                                 token-source context tok))
              (parent-source token-source)))))

  (defmacro next-char/shift (token-source-location context)
    "Extract and return just one character from the source at
     TOKEN-SOURCE-LOCATION, at which then store a copy of the source with
     offset(s) shifted to just after that character."
    (with-ps-gensyms (c)
      `(loop for ,c := (next-char ,token-source-location ,context)
             do (setf ,token-source-location
                        (source-after-next-char ,token-source-location
                                                ,context))
             when (or ,c (not ,token-source-location)) return ,c)))

  (defmethod input-string ((tsrc token-source))
    (declare (special *root-context*))
    (loop for tok := (next-token/shift tsrc *root-context*)
	  for str := (input-string tok)
	          :then (concatenate 'string str (input-string tok))
	  finally (return str)))

  (defstruct (parser-state (:conc-name))
    "A container for various kinds of parser states that may include some
     accumulated token material, a terminator token, a non-token value, an
     error display, and a token source positioned after the input consumed
     to produce those details."
    accumulator terminator parser-value parser-error token-source-state)

  (defun parser-accumulator-state (token-source accumulator
                                   &optional terminator)
    "Return a parser state with accumulated material and an optional
     terminator."
    (make-parser-state :token-source-state token-source
                       :accumulator accumulator
                       :terminator terminator))

  (defun parser-error-state (token-source error)
    "Return a parser state with an error."
    (make-parser-state :token-source-state token-source
                       :error error))

  (defmacro parser-error-state* (token-source &rest error-init)
    "Return a parser state with an error, instantiated with ERROR-DISPLAY*
     syntax."
    `(parser-error-state ,token-source (error-display* ,@error-init)))

  (defmacro parser-state-bind ((&key accumulator terminator value error
                                     token-source)
                               state
                               &body body)
    (with-var-value (state)
      `(let (,@(and accumulator `((,accumulator (accumulator ,state))))
             ,@(and terminator `((,terminator (terminator ,state))))
             ,@(and value `((,value (parser-value ,state))))
             ,@(and error `((,error (parser-error ,state))))
             ,@(and token-source `((,token-source
                                    (token-source-state ,state)))))
             
         ,@body)))

)

;;; Local Variables: ***
;;; mode:lisp ***
;;; local-font-lock-keywords:(("(\\(def\\(?:\\(ps\\(?:macro\\|fun\\)\\|\\(?:un\\|macro\\)\\+ps\\)\\|\\(guard\\|enum\\)\\|\\(struct-guarded\\)\\)\\)[ \t\n]+(?\\([^()]*?\\)[ \t\n]" (1 font-lock-keyword-face) (5 (cond ((match-beginning 2) font-lock-function-name-face) ((match-beginning 3) font-lock-variable-name-face) ((match-beginning 4) font-lock-type-face)))) ("(\\(ambi-ps\\)\\_>" (1 font-lock-preprocessor-face))) ***
;;; End: ***
