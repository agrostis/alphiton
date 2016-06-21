(in-package #:cl-user)

(defpackage #:alphiton-test
  (:use #:cl #:alphiton #:eos)
  (:export #:run-tests))

(in-package #:alphiton-test)

(defun pprint-for-test (expr out)
  (let ((*print-pretty* t))
    (case (first expr)
      ((defparameter)
       (format out "~:<~1I~(~W ~W~) ~_~W~:>~%" expr))
      ((test)
       (format out "~(~:<~1I~W ~W~@{ ~_~W~^~}~:>~)~%" expr))
      (t (write expr :stream out)))))

(defun format-tests (tests)
  (let ((*package* (find-package '#:alphiton-test)))
    (with-output-to-string (out)
      (loop initially (terpri out)
            for (test-name source-file source-line
                 used-tests bindings alphiton json)
              in tests
            with history
            for $alphiton := (gentemp "_ALPHITON_")
            for $json := (gentemp "_JSON_")
            for $$used :=
              (loop for name :in used-tests
                    for $alphiton := (second (assoc name history))
                    if $alphiton collect $alphiton)
            do (push (list test-name $alphiton $json) history)
               (format out "~&;;; @TEST ~A [~A:~A]~%~%"
                       test-name source-file source-line)
               (pprint-for-test `(defparameter ,$alphiton ,alphiton) out)
               (terpri out)
               (pprint-for-test `(defparameter ,$json ,json) out)
               (terpri out)
               (let* ((alphiton-call-0 (if $$used
                                           `(alphiton-using (list ,@$$used)
                                                            ,$alphiton)
                                           `(alphiton ,$alphiton)))
                      (alphiton-call (if bindings
                                    `(let ,bindings ,alphiton-call-0)
                                    alphiton-call-0))
                      (json-call `(funcall *dom-root-wrapper*
                                           (json-to-dom ,$json))))
                 (pprint-for-test
                   `(test ,test-name
                          (is (dom-equal% ,json-call ,alphiton-call)))
                   out))
               (terpri out)))))

(defun format-js-tests (tests)
  (with-output-to-string (out)
    (loop for (test-name source-file source-line
               used-tests bindings alphiton json)
            in tests
          unless bindings
            do (format out "~%    ~S: {~
                            ~%        use: [~{~S~^, ~}],~
                            ~%        alphiton: ~A,~
                            ~%        reference: ~A~
                            ~%    },~%"
                       (ps:symbol-to-js-string test-name)
                       (mapcar #'ps:symbol-to-js-string used-tests)
                       (car (ps::parenscript-print alphiton nil))
                       json))))

(defun alphiton-using (used source)
  (loop for used-source :in used
        for ctx := (alphiton used-source nil t)
                :then (alphiton used-source nil ctx)
        finally (let ((*root-context* ctx))
                  (return (alphiton source)))))

(defun format-as-error-json (input messages counter)
  (assert (every #'stringp messages))
  (let ((eotp nil))
    (when (and (consp input) (eq (first (last input)) :eot))
      (setq input (apply #'concatenate 'string (butlast input))
            eotp t))
    (with-output-to-string (out)
      (let ((json:*json-output* out))
        (dom-to-json
          (funcall *dom-error-wrapper*
            input eotp (coerce messages 'vector)
            (format nil "AlphitonError~A" counter)))))))

(defun get-test-templates (sources)
  (let ((tests nil))
    (format *error-output*
            "~&~%; reading test templates from ~{~A~^, ~}...~%"
            (mapcar #'enough-namestring sources))
    (linewise-template:process-template
        ((:files sources :circumfix '("\\s*" "\\s*"))
             :discard
             ((:block "TEST" test-name
                      &aux used-tests bindings alphiton json
                           (source-file
                            (file-namestring
                              (pathname linewise-template:*source-stream*)))
                           (source-line
                            linewise-template:*source-line-count*)
                           (error-counter -1))
                  :discard
                  :do (flet ((mt (s) (intern (string s) '#:alphiton-test))
                             (rt (s) (cl-ppcre:regex-replace "\\n$" s "")))
                        (push (list (mt test-name) source-file source-line
                                    (mapcar #'mt used-tests)
                                    bindings (rt alphiton) (rt json))
                              tests))
                  ((:atomic "USE" used-test)
                       :discard :do (push used-test used-tests))
                  ((:after "ALPHITON" &optional bndngs)
                       :copy :to (:var alphiton) :do (setq bindings bndngs))
                  ((:after "JSON")
                       :copy :to (:var json)
                       ((:atomic "ERROR IN" input messages &trailer trlr)
                        :replace :with (concatenate 'string
                                         (format-as-error-json
                                           input messages
                                           (incf error-counter))
                                         trlr))))))
    (nreverse tests)))

(defun generate-tests (destination sources)
  (let ((tests (get-test-templates sources)))
    (format *error-output*
            "~&~%; writing generated tests to ~S...~%"
            (enough-namestring destination))
    (linewise-template:process-template
        ((:file destination :update t :circumfix '("\\s*;;;\\s*"))
             :copy
             ((:block "GENERATED-TESTS")
                  :replace :with (format-tests tests)
                  :preserve-directives t)))))

(defun generate-js-tests (destination sources)
  (let ((tests (get-test-templates sources)))
    (format *error-output*
            "~&~%; writing generated js tests to ~S...~%"
            (enough-namestring destination))
    (linewise-template:process-template
        ((:file destination :update t :circumfix '("\\s*/(\\*{8,}) " " \\1/"))
             :copy
             ((:block "TESTS")
                  :replace :with (format-js-tests tests)
                  :preserve-directives t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite alphiton-tests)

(defun run-tests ()
  (format t "Running Alphiton tests:~&")
  (run! 'alphiton-tests))
