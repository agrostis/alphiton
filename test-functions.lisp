(in-package #:cl-user)

(defpackage #:mex-test 
  (:use #:cl #:mex #:eos)
  (:export #:run-tests))

(in-package #:mex-test)

(defun copy-destination-until-marker (destination out)
  (with-open-file (in destination)
    (loop for line := (read-line in nil nil)
          while line
          with marker := "@BEGIN GENERATED-TESTS"
          do (write-line line out)
          if (search marker line) do (terpri out) and return nil
          finally (format out "~&~%;;; ~A~%~%" marker))))

(defmacro with-output-to-generated-tests-destination ((stream destination)
                                                      &body body)
  `(let ((generated
          (with-output-to-string (,stream)
            (copy-destination-until-marker ,destination ,stream)
            ,@body)))
     (format *error-output*
             "~&~%; writing generated tests to ~S...~%"
             (enough-namestring ,destination))
     (with-open-file (,stream ,destination
                      :direction :output :if-exists :supersede)
       (princ generated ,stream))))

(defun format-as-error-json (input eotp messages counter)
  (with-output-to-string (out)
    (let ((json:*json-output* out))
      (dom-to-json%
        (funcall *dom-error-wrapper*
          input eotp messages
          (format nil "MexError~A" counter))))))

(defun parse-for-tests (in process-test)
  (labels ((read-after (line pos &optional default)
             (multiple-value-bind (datum pos)
                 (ignore-errors
                   (with-input-from-string (in line)
                     (file-position in pos)
                     (values (read in) (file-position in))))
               (values (or datum default) pos)))
           (read-after-marker-match (line pos marker &optional default)
             (read-after line (+ pos (length marker)) default))
           (blankp (c)
             (case c ((#\Space #\Tab) t) (t nil)))
           (dispatch-on-test-begin (line line-number)
             (let* ((test-begin-marker "@BEGIN TEST")
                    (pos (search test-begin-marker line)))
               (when pos
                 (let ((test-id
                         (read-after-marker-match
                           line pos test-begin-marker
                           (gentemp "TEST"))))
                   (list 'test `(:id ,test-id :line ,line-number))))))
           (dispatch-on-use (line props)
             (let* ((usage-marker "@USE")
                    (pos (search usage-marker line)))
               (when pos
                 (let ((test-id (read-after-marker-match
                                  line pos usage-marker))
                       (uses (getf props :use)))
                   (pushnew test-id uses)
                   (setf (getf props :use) uses)
                   (list 'test props)))))
           (dispatch-on-mex (line props)
             (let* ((mex-begin-marker "@MEX")
                    (pos (search mex-begin-marker line)))
               (when pos
                 (let ((bindings (read-after-marker-match
                                   line pos mex-begin-marker))
                       (mex-out (make-string-output-stream)))
                   (list 'begin-mex
                         `(,@props :mex ,mex-out :bindings ,bindings))))))
           (dispatch-on-json (line props)
             (let ((json-begin-marker "@JSON")) 
                (when (search json-begin-marker line)
                  (let ((json-out (make-string-output-stream)))
                    (list 'begin-json `(,@props :json ,json-out))))))
           (dispatch-on-error (line props)
             (let* ((error-marker "@ERROR IN")
                    (eot-marker "@EOT")
                    (error-pos (search error-marker line)))
               (when error-pos
                 (multiple-value-bind (input input-end-pos)
                     (read-after-marker-match line error-pos error-marker)
                   (when (stringp input)
                     (let ((eot-pos (search eot-marker line
                                      :start2 input-end-pos
                                      :end2 (+ (position-if-not #'blankp
                                                 line :start input-end-pos)
                                               (length eot-marker)))))
                       (multiple-value-bind (messages messages-end-pos)
                           (if eot-pos
                               (read-after-marker-match
                                 line eot-pos eot-marker)
                               (read-after line input-end-pos))
                         (when (and messages
                                    (ignore-errors
                                      (every #'stringp messages)))
                           (let* ((count (getf props :error-count 0))
                                  (line* (concatenate 'string
                                           (format-as-error-json
                                             input eot-pos messages count)
                                           (subseq line messages-end-pos))))
                             (setf (getf props :error-count) (1+ count))
                             (list 'json props line*))))))))))
           (dispatch-on-test-end (line props)
             (let ((test-end-marker "@END TEST"))
               (when (search test-end-marker line)
                 (list 'end-test props)))))
    (loop for line := (read-line in nil nil)
          while line
          for line-number :upfrom 1
          for (state props line*)
            := (case state
                 ((nil end-test)
                  (or (dispatch-on-test-begin line line-number)
                      (list nil nil)))
                 ((test)
                  (or (dispatch-on-use line props)
                      (dispatch-on-mex line props)
                      (list 'test props)))
                 ((mex begin-mex)
                  (or (dispatch-on-json line props)
                      (list 'mex props)))
                 ((json begin-json)
                  (or (dispatch-on-error line props)
                      (dispatch-on-test-end line props)
                      (list 'json props))))
         if (eq state 'mex)
           do (write-line (or line* line) (getf props :mex))
         else if (eq state 'json)
           do (write-line (or line* line) (getf props :json))
         else if (eq state 'end-test)
           do (dolist (prop '(:mex :json))
                (let* ((text (get-output-stream-string (getf props prop)))
                       (last-i (1- (length text))))
                  (setf (getf props prop)
                          (if (char= (aref text last-i) #\Newline)
                              (subseq text 0 last-i)
                              text))))
              (remf props :error-count)
              (apply process-test props))))

(defun pprint-for-test (expr out)
  (let ((*print-pretty* t))
    (case (first expr)
      ((defparameter)
       (format out "~:<~1I~(~W ~W~) ~_~W~:>~%" expr))
      ((test)
       (format out "~(~:<~1I~W ~W~@{ ~_~W~^~}~:>~)~%" expr))
      (t (write expr :stream out)))))

(defun generate-tests-1 (filename in out)
  (parse-for-tests in
    (let ((history '()))
      (lambda (&key id line use mex bindings json)
        (let ((mexv (gentemp "_MEX_"))
              (jsonv (gentemp "_JSON_"))
              (used-mexvs
                (loop for id :in use
                      for mexv := (second (assoc id history))
                      if mexv collect mexv)))
          (push (list id mexv jsonv) history)
          (format out "~&~%;;; @TEST ~A [~A:~A]~%~%" id filename line)
          (pprint-for-test `(defparameter ,mexv ,mex) out)
          (terpri out) 
          (pprint-for-test `(defparameter ,jsonv ,json) out)
          (terpri out)
          (let* ((mex-call-0 (if used-mexvs
                                 `(mex-using (list ,@used-mexvs) ,mexv)
                                 `(mex ,mexv)))
                 (mex-call (if bindings
                               `(let ,bindings ,mex-call-0)
                               mex-call-0))
                 (json-call `(funcall *dom-root-wrapper*
                                      (json-to-dom% ,jsonv))))
            (pprint-for-test
              `(test ,id (is (dom-equal% ,json-call ,mex-call)))
              out))
          (terpri out))))))

(defun mex-using (used source)
  (loop for used-source :in used
        for ctx := (mex used-source t) :then (mex used-source ctx)
        finally (let ((*root-context* ctx))
                  (return (mex source)))))

(defun generate-tests (destination sources)
  (let ((*package* (find-package '#:mex-test)))
    (with-output-to-generated-tests-destination (out destination)
      (dolist (source sources)
        (with-open-file (in source)
          (generate-tests-1 (file-namestring source) in out)))
      (format out "~&~%;;; @END GENERATED-TESTS~%"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite mex-tests)

(defun run-tests ()
  (format t "Running Mex tests:~&")
  (run! 'mex-tests))
