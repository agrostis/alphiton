(in-package #:cl-user)

(defpackage #:mex-test 
  (:use #:cl #:mex #:eos)
  (:export #:run-tests))

(in-package #:mex-test)

(defun pprint-for-test (expr out)
  (let ((*print-pretty* t))
    (case (first expr)
      ((defparameter)
       (format out "~:<~1I~(~W ~W~) ~_~W~:>~%" expr))
      ((test)
       (format out "~(~:<~1I~W ~W~@{ ~_~W~^~}~:>~)~%" expr))
      (t (write expr :stream out)))))

(defun format-tests (tests)
  (let ((*package* (find-package '#:mex-test)))
    (with-output-to-string (out)
      (loop initially (terpri out)
            for (test-name source-file source-line
                 used-tests bindings mex json)
              in tests
            with history
            for $mex := (gentemp "_MEX_")
            for $json := (gentemp "_JSON_")
            for $$used :=
              (loop for name :in used-tests
                    for $mex := (second (assoc name history))
                    if $mex collect $mex)
            do (push (list test-name $mex $json) history)
               (format out "~&;;; @TEST ~A [~A:~A]~%~%"
                       test-name source-file source-line)
               (pprint-for-test `(defparameter ,$mex ,mex) out)
               (terpri out)
               (pprint-for-test `(defparameter ,$json ,json) out)
               (terpri out)
               (let* ((mex-call-0 (if $$used
                                      `(mex-using (list ,@$$used) ,$mex)
                                      `(mex ,$mex)))
                      (mex-call (if bindings
                                    `(let ,bindings ,mex-call-0)
                                    mex-call-0))
                      (json-call `(funcall *dom-root-wrapper*
                                           (json-to-dom ,$json))))
                 (pprint-for-test
                   `(test ,test-name (is (dom-equal% ,json-call ,mex-call)))
                   out))
               (terpri out)))))

(defun mex-using (used source)
  (loop for used-source :in used
        for ctx := (mex used-source t) :then (mex used-source ctx)
        finally (let ((*root-context* ctx))
                  (return (mex source)))))

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
            (format nil "MexError~A" counter)))))))

(defun get-test-templates (sources)
  (let ((tests nil))
    (format *error-output*
            "~&~%; reading test templates from ~{~A~^, ~}...~%"
            (mapcar #'enough-namestring sources))
    (linewise-template:process-template
        ((:files sources :circumfix '("\\s*" "\\s*"))
             :discard
             ((:block "TEST" test-name
                      &aux used-tests bindings mex json
                           (source-file
                            (file-namestring
                              (pathname linewise-template:*source-stream*)))
                           (source-line
                            linewise-template:*source-line-count*)
                           (error-counter -1))
                  :discard
                  :do (flet ((mt (s) (intern (string s) '#:mex-test))
                             (rt (s) (cl-ppcre:regex-replace "\\n$" s "")))
                        (push (list (mt test-name) source-file source-line
                                    (mapcar #'mt used-tests)
                                    bindings (rt mex) (rt json))
                              tests))
                  ((:atomic "USE" used-test)
                       :discard :do (push used-test used-tests))
                  ((:after "MEX" &optional bndngs)
                       :copy :to (:var mex) :do (setq bindings bndngs))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite mex-tests)

(defun run-tests ()
  (format t "Running Mex tests:~&")
  (run! 'mex-tests))
