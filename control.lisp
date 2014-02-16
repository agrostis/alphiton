(in-package #:mex)

;;; Control features

(ambi-ps ()

  ;; Conditional expansion

  (defun match-conditional-check-errors (branches)
    "Check material collected by MATCH-CONDITIONAL for common syntax errors.
     Return an error display, or null, if the syntax is correct."
    (let ((errors nil)
          (ellip (tokens :chars "...")))
      (flet ((add-error (ed sep)
               (setf errors
                     (if errors
                         (if sep
                             (error-display*
                               :add-to errors :append-input sep :append ed)
                             (error-display*
                               :add-to errors :append ed))
                         ed))))
        (loop for branch :across branches
              for head := (aref branch 0)
              with sep := nil
              with past-else := nil
              if past-else
                do (add-error
                     (error-display* "extraConditionalBranch" branch)
                     sep)
                   (setf sep nil)
              else if (token-is head :command "else")
                do (setf past-else t sep ellip)
              else if (= (length branch) 1)
                do (add-error
                     (error-display* "emptyConditionalBranch" head)
                     sep)
                   (setf sep nil)
              else
                do (setf sep ellip)))
      errors))

  (defun match-conditional (match context)
    "Consume input without expansion, collecting conditional branches.
     Return MATCH with its accumulator set to hold the vector of branches."
    (loop
      for input := (next-token/shift (token-source-state match) context)
      with nest-level := 0
      with current-branch := (stack (dispatching-token match))
      if (eot-p input)
        return nil
      else
        do (incf (match-length match))
      if (token-is input :command "if")
        do (incf nest-level)
           (incf (matched-token-count match))
      else if (token-is input :command "endif")
        do (decf nest-level)
           (incf (matched-token-count match))
      when (< nest-level 0)
        collect (ensure-vector current-branch) :into branches
        and return (let ((branches (ensure-vector branches)))
                     (setf (parser-error match)
                             (match-conditional-check-errors branches)
                           (accumulator match)
                             branches)
                     match)
      if (and (= nest-level 0)
              (or (token-is input :command "elsif")
                  (token-is input :command "else")))
        collect (ensure-vector current-branch) :into branches
        and do (setf current-branch (stack input))
               (incf (matched-token-count match))
      else
        do (stack-push input current-branch)))

  (defun try-conditional-branch (branch context token-source)
    "If BRANCH starts with \\else, unconditionally return all tokens after
     \\else.  Otherwise, consume one dispatching token in BRANCH after \\if
     or \\elsif, subject to parameter expansion.  Expand the command, which
     must result in a data object being disposed of, and may consume more
     tokens from the branch.  If the command expands correctly and disposes
     of generalized boolean truth, return the unconsumed part of the branch.
     Otherwise, return null (or an error display)."
    (let ((branch-head (aref branch 0))
          (branch-sans-head (spliced branch 0 1)))
      (if (token-is branch-head :command "else")
          branch-sans-head
          (let ((tsrc (expansion-to-token-source
                        branch-sans-head nil
                        (or (expansion-context token-source)
                            context))))
            (if-match-bind ((cmd-tok token* #'dispatching-token-p))
                tsrc context
              (let* ((got-condition nil)
                     (condition-value nil)
                     (*verify-group-balance* t)
                     (*put-data-callback*
                      (lambda (value ctx)
                        (declare (ignore ctx))
                        (setf got-condition t condition-value value))))
                (parser-state-bind (:token-source tsrc+)
                    (mex-dispatch cmd-tok tsrc context nil)
                  (cond
                    ((not got-condition)
                     (error-display* "noData" branch))
                    (condition-value
                     (loop for tok := (next-token/shift tsrc+ context t)
                           until (eot-p tok)
                           collect tok :into expn
                           finally (return (ensure-vector expn))))
                    (t nil))))
              (error-display* "noData" branch))))))

  (defbuiltin if (:match match :context ctx)
    "Consume a conditional expression, consisting of ``branches'' delimited
     by properly balanced \\if...[\\elsif...\\else...]\\endif tokens.
     Find the first \\if... or \\elsif... branch whose initial tokens form a
     command (``condition'') that disposes of a non-false datum, and expand
     to the rest of the branch.  If all conditions dispose of false, but
     there is an \\else... branch, expand to that branch (sans \\else).
     Otherwise, expand to nothing."
    :pattern
    (match-conditional match ctx)
    :handler
    (if (parser-error match)
        match
        (parser-state-bind (:accumulator branches :token-source tsrc)
            match
          (loop for branch :across branches
                for expn := (try-conditional-branch branch ctx tsrc)
                when expn
                  return (if (error-display-p expn)
                             (parser-error-state tsrc expn)
                             (parser-expansion-state tsrc expn))
                finally
                  (return (parser-expansion-state tsrc t))))))


  ;; Iterated expansion

  (defun match-iteration-body (match context)
    "Consume input without expansion, collecting material which constitutes
     an iteration body.  Return MATCH with its accumulator set to hold the
     body."
    (loop for input := (next-token/shift (token-source-state match) context)
          with nest-level := 0
          with body := (stack)
          if (eot-p input)
            return nil
          else
            do (incf (match-length match))
          if (token-is input :command "for")
            do (incf nest-level)
          else if (token-is input :command "endfor")
            do (decf nest-level)
          if (< nest-level 0)
            do (setf (accumulator match) (ensure-vector body))
            and return match
          else
            do (stack-push input body)))

  (defun run-iteration (iterator context ship)
    (let ((iter-ctx (spawn-context context
                      :command-table (make-table)
                      :parent-context nil)))
      (add-command (command-key 'break)
                   (make-builtin :pattern (vector)
                                 :handler (lambda (m c s)
                                            (declare (ignore m c s))
                                            (return-from run-iteration)))
                   (command-table iter-ctx))
      (let ((*group-end-p* #'eot-p)
            (*verify-group-balance* nil))
        (funcall iterator
          (lambda (token-source context)
            (setf (parent-context iter-ctx) context)
            (get-group-contents token-source iter-ctx t ship))))))

  (defun iterate-over-data (iterator reg-command match context ship)
    (let ((body-tsrc (expansion-to-token-source
                       (accumulator match) nil (match-context match))))
      (run-iteration
        (if reg-command
            (let* ((cmd-table (make-table))
                   (reg-table (make-table))
                   (lctx (spawn-context context
                           :command-table cmd-table
                           :register-table reg-table)))
              (setf reg-command (token-command-key reg-command))
              (add-register-commands reg-command cmd-table reg-table)
              (lambda (callback)
                (funcall iterator
                  (lambda (datum)
                    (remember reg-command reg-table datum)
                    (funcall callback body-tsrc lctx)))))
            (lambda (callback)
              (funcall iterator
                (lambda (datum)
                  (context-stack-push datum context)
                  (funcall callback body-tsrc context)))))
        context ship)))

  (defun iterate-over-input (iterator pattern match context ship)
    (let ((expander (make-macro
                      :pattern (or pattern (vector))
                      :expansion (accumulator match)))
          (mctx (match-context match)))
      (run-iteration
        (lambda (callback)
          (funcall iterator
            (lambda (input)
              (let* ((input-tsrc (expansion-to-token-source input nil mctx))
                     (body-tsrc (mex-expand (vector expander) nil input-tsrc
                                            context nil)))
                (funcall callback body-tsrc context)))))
        context ship)))

  (defbuiltin for (reg-cmd start end step
                   :match match :context ctx :ship ship)
    "Consume [<Reg>=]\\ints <Start>\\to <End>[\\by <Step>]<Body>\\endfor,
     where <Reg> is a command token, <Start>, <End> and <Step> are integer
     specs (such as may be used with \\int), and <Body> is a sequence of
     tokens with properly balanced \\for...\\endfor.  Iterate over integers
     from <Start> to <End> with step <Step> (by default, use step
     Signum(<End> - <Start>) ).  If register name <Reg> is given, register
     commands shall be added to the iterator context, and on each iteration
     the register shall be updated to hold the next integer value;
     otherwise, the value shall be pushed to the stack on every iteration."
    :pattern
    (and (match-setf-update ((? (reg-cmd token #'command-token-p)
                                (:chars "="))
                             (:command "ints"))
             match ctx)
         (when (match-int match ctx) (setf start (accumulator match)) t)
         (match-setf-update ((:command "to")) match ctx)
         (when (match-int match ctx) (setf end (accumulator match)) t)
         (or (not (match-setf-update ((:command "by")) match ctx))
             (when (match-int match ctx) (setf step (accumulator match)) t))
         (match-iteration-body match ctx)
         (yield match reg-cmd start end step))
    :handler
    (if (numberp step)
        (when (or (= step 0) (not (eq (>= end start) (> step 0))))
          (return-from for
            (parser-error-state* (token-source-state match)
              "invalidStep" (string-to-input (ensure-string step) ctx))))
        (setf step (if (>= end start) 1 -1)))
    (iterate-over-data
      (lambda (callback)
        (loop for i :from start :to end :by step
              do (funcall callback i)))
      reg-cmd match ctx ship))

  (defbuiltin for (reg-cmd start end
                   :match match :context ctx :ship ship)
    "Consume [<Reg>=]\\chars <Start>\\to <End><Body>\\endfor, where <Reg> is
     a command token, <Start> and <End> are character tokens, and <Body> is
     a sequence of tokens with properly balanced \\for...\\endfor.  Iterate
     over characters from <Start> to <End>.  If register name <Reg> is
     given, register commands shall be added to the iterator context, and on
     each iteration the register shall be updated to hold the next token
     value; otherwise, the value shall be pushed to the stack on every
     iteration."
    :pattern
    (and (match-setf-update ((? (reg-cmd token #'command-token-p)
                                (:chars "="))
                             (:command "chars")
                             (start token* #'char-token-p)
                             (:command "to")
                             (end token* #'char-token-p))
             match ctx)
         (match-iteration-body match ctx)
         (yield match reg-cmd start end))
    :handler
    (let* ((start-code (char-code (token-chr start)))
           (end-code (char-code (token-chr end)))
           (step (if (>= end-code start-code) 1 -1)))
      (iterate-over-data
        (lambda (callback)
          (loop for i :from start-code :to end-code :by step
                for c := (code-char i)
                for tok := (make-char-token :chr c :context ctx
                                            :category (char-cat c))
                do (funcall callback tok)))
        reg-cmd match ctx ship)))

)

;;; Local Variables: ***
;;; mode:lisp ***
;;; local-font-lock-keywords:(("(\\(def\\(?:\\(ps\\(?:macro\\|fun\\)\\|\\(?:un\\|macro\\)\\+ps\\|builtin\\(?:-iterator\\)?\\)\\|\\(guard\\|enum\\)\\|\\(struct-guarded\\)\\)\\)[ \t\n]+(?\\([^()]*?\\)[ \t\n]" (1 font-lock-keyword-face) (5 (cond ((match-beginning 2) font-lock-function-name-face) ((match-beginning 3) font-lock-variable-name-face) ((match-beginning 4) font-lock-type-face)))) ("(\\(ambi-ps\\)\\_>" (1 font-lock-preprocessor-face))) ***
;;; End: ***
