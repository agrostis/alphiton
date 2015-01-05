(in-package #:mex)

;;; Control features

(ambi-ps ()

  ;; Conditional expansion

  (defun match-conditional-check-errors (semibranches)
    "Check material collected by MATCH-CONDITIONAL for common syntax errors.
     Return an error display, or null, if the syntax is correct."
    (let ((errors nil)
          (ellip (tokens% :chars "...")))
      (flet ((add-error (ed sep)
               (setf errors (error-display*
                              :add-to errors :sep sep :append ed))))
        (loop for semibranch :across semibranches
              for head := (aref semibranch 0)
              for expect-condition := t :then (not expect-condition)
              with past-else := nil
              with sep := nil
              if past-else
                do (add-error
                     (error-display* "extraConditionalBranch" semibranch)
                     sep)
                   (setf sep nil)
              else if (eq expect-condition
                          (token-is head :command "then"))
                do (add-error
                     (if expect-condition
                         (error-display* "missingCondition" semibranch)
                         (error-display* "missingConditionalBody" semibranch))
                     sep)
                   (setf sep nil)
              else if (token-is head :command "else")
                do (setf past-else t sep ellip)
              else if (and expect-condition (< (length semibranch) 2))
                do (add-error
                     (error-display* "emptyCondition" semibranch)
                     sep)
                   (setf sep nil)
              else
                do (setf sep ellip)))
      errors))

  (defun match-conditional (match context)
    "Consume input without expansion, collecting conditional branches.
     Return MATCH with its accumulator set to hold the vector of
     semibranches (runs of tokens beginning with \\if, \\elsif, \\then, or
     \\else)."
    (loop
      for input := (next-token/shift (token-source-state match) context)
      with nest-level := 0
      with current-semibranch := (stack (dispatching-token match))
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
        collect (ensure-vector current-semibranch) :into semibranches
        and return (let ((semibranches (ensure-vector semibranches)))
                     (setf (parser-error match)
                             (match-conditional-check-errors semibranches)
                           (accumulator match)
                             semibranches)
                     match)
      if (and (= nest-level 0)
              (or (token-is input :command "elsif")
                  (token-is input :command "else")
                  (token-is input :command "then")))
        collect (ensure-vector current-semibranch) :into semibranches
        and do (setf current-semibranch (stack input))
               (incf (matched-token-count match))
      else
        do (stack-push input current-semibranch)))

  (defun try-conditional-branch (condition body context token-source)
    "If CONDITION is null (which is the case for an \\else branch), return
     the tokens of BODY except the initial \\else.  Otherwise, expand tokens
     in CONDITION after \\if or \\elsif, which should cause at least one
     data object to be pushed to the stack.  If the last object disposed
     of (the “condition value”) is generalized boolean truth, return the
     tokens of BODY except the initial \\then.  Otherwise, return NULL.  In
     either case, pop the condition value off the stack."
    (let ((got-condition nil)
          (condition-value t))
      (if condition
          (let ((*put-data-callback*
                 (lambda (value ctx)
                   (setf got-condition t condition-value value)
                   (context-stack-push value ctx))))
            (parser-state-bind (:error errors)
                (get-full-expansion (spliced condition 0 1) token-source
                                    context t)
              (with-context-dom-stack (stacks context :current current)
                (when (and got-condition
                           (not (stack-empty-p current))
                           (eq (stack-peek current) condition-value))
                  (stack-pop current)))
              (cond
                (errors)
                ((not got-condition) (error-display* "noData" condition))
                (t (and (not (false-p condition-value))
                        (spliced body 0 1))))))
          (spliced body 0 1))))

  (defbuiltin if (:match match :context ctx)
    "Consume a conditional expression, consisting of ``branches'' delimited
     by properly balanced \\if<CONDITION>\\then<BODY>[\\elsif<CONDITION>
     \\then<BODY>...\\else<BODY>]\\endif, where <CONDITION> and <BODY> are
     token runs. Find the first \\if... or \\elsif... branch whose
     <CONDITION> includes a command that disposes of a non-false datum, and
     expand to the corresponding <BODY>.  If all conditions dispose of
     false, but there is an \\else... branch, expand to the <BODY> that
     follows \\else.  Otherwise, expand to nothing."
    :pattern
    (match-conditional match ctx)
    :handler
    (if (parser-error match)
        match
        (parser-state-bind (:accumulator semibranches :token-source tsrc)
            match
          (loop for i := 0 :then (+ i 2)
                with len := (length semibranches)
                while (< i len)
                for condition := (and (< (1+ i) len) (aref semibranches i))
                for body :=  (aref semibranches (if condition (1+ i) i))
                for expn := (try-conditional-branch condition body ctx tsrc)
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

  (defun run-iteration (iterator lexical-context ship)
    (add-command (command-key 'break)
                 (make-builtin :pattern (vector)
                               :handler (lambda (m c s)
                                          (declare (ignore m c s))
                                          (return-from run-iteration)))
                 (command-table lexical-context))
    (let ((*group-end-p* #'eot-p)
          (*verify-group-balance* nil))
      (funcall iterator
        (lambda (token-source context)
          (get-group-contents token-source context t ship)))))

  (defmacro with-temp-interlay-context ((temp-context context) &body body)
    (with-var-value (context)
      (with-ps-gensyms (pctx cmd-table reg-table lctx)
        `(let* ((,pctx (parent-context ,context))
                (,cmd-table (make-table))
                (,reg-table (make-table))
                (,temp-context (spawn-context ,context
                                 :parent-context ,pctx
                                 :command-table ,cmd-table
                                 :register-table ,reg-table)))
           (setf (parent-context ,context) ,temp-context)
           (unwind-protect (progn ,@body)
             (setf (parent-context ,context) ,pctx))))))

  (defun iterate-over-data (iterator reg-command match context ship)
    (let ((body-tsrc (expansion-to-token-source
                       (accumulator match) nil (match-context match))))
      (with-temp-interlay-context
          (lctx (token-context (or reg-command (dispatching-token match))))
        (if reg-command
            (let ((cmd-table (command-table lctx))
                  (reg-table (register-table lctx)))
              (setf reg-command (token-command-key reg-command))
              (add-register-commands reg-command cmd-table reg-table)
              (run-iteration
                (lambda (callback)
                  (funcall iterator
                    (lambda (datum)
                      (remember reg-command reg-table datum)
                      (funcall callback body-tsrc context))))
                lctx ship))
            (run-iteration
              (lambda (callback)
                (funcall iterator
                  (lambda (datum)
                    (context-stack-push datum context)
                    (funcall callback body-tsrc context))))
              lctx ship)))))

  (defun iterate-over-input (iterator pattern match context ship)
    (let ((expander (make-macro
                      :pattern (or pattern (vector))
                      :expansion (accumulator match)))
          (mctx (match-context match)))
      (with-temp-interlay-context
          (lctx (token-context (dispatching-token match)))
        (run-iteration
          (lambda (callback)
            (funcall iterator
              (lambda (input)
                (let* ((input-tsrc (expansion-to-token-source
                                     input nil mctx))
                       (body-tsrc (mex-expand (vector expander) nil
                                    input-tsrc context nil)))
                  (funcall callback body-tsrc context)))))
          lctx ship))))

  (defbuiltin for (reg-cmd start end step
                   :match match :context ctx :ship ship :token-source tsrc)
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
    (block nil
      (if (numberp step)
          (when (or (= step 0) (not (eq (>= end start) (> step 0))))
            (return
              (parser-error-state* tsrc
                "invalidStep" (string-to-input (ensure-string step) ctx))))
          (setf step (if (>= end start) 1 -1)))
      (iterate-over-data
        (lambda (callback)
          (loop for i :from start :to end :by step
                do (funcall callback i)))
        reg-cmd match ctx ship)
      (parser-expansion-state tsrc t)))

  (defbuiltin for (reg-cmd start end
                   :match match :context ctx :ship ship :token-source tsrc)
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
        reg-cmd match ctx ship)
      (parser-expansion-state tsrc t)))

)

#|
@BEGIN TEST CONTROL1
@MEX
\insignificantWhitespaces
\def\multipleof#N{\int#N\rem\int0\eq}
\for\x=\ints 2 \to 4
  \for\ints 7 \to 9
    \if \dup\x\rem\int3\geq \then \break
    \elsif \dup\x\mul\multipleof3 \then \pop*\x\pop\blank
    \elsif \dup\x\add\multipleof5 \then \pop+\x\pop\blank
    \else \discard
    \endif
  \endfor
\endfor
@JSON
{"t": "8+2 9*2 7*3 8*3 9*3 "},
@END TEST
|#

;;; Local Variables: ***
;;; mode:lisp ***
;;; local-font-lock-keywords:(("(\\(def\\(?:\\(ps\\(?:macro\\|fun\\)\\|\\(?:un\\|macro\\)\\+ps\\|builtin\\(?:-iterator\\)?\\)\\|\\(guard\\|enum\\)\\|\\(struct-guarded\\)\\)\\)[ \t\n]+(?\\([^()]*?\\)[ \t\n]" (1 font-lock-keyword-face) (5 (cond ((match-beginning 2) font-lock-function-name-face) ((match-beginning 3) font-lock-variable-name-face) ((match-beginning 4) font-lock-type-face)))) ("(\\(ambi-ps\\)\\_>" (1 font-lock-preprocessor-face))) ***
;;; End: ***
