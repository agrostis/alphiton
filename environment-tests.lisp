(in-package #:mex-test)

(in-suite mex-tests)

(defun rand-string (length &optional make-substring)
  "Return a string of random characters.  The range of characters is between
   0 and 255, which is subdivided into four subranges of equal width.  The
   probability of a character appearing in the string decreases
   exponentially from lowest to highest subrange.  If MAKE-SUBSTRING is
   supplied, the character is fed into it to produce a string."
  (with-output-to-string (out)
    (let ((putc (if make-substring
                    (lambda (c) (princ (funcall make-substring c) out))
                    (lambda (c) (write-char c out)))))
      (dotimes (i length)
        (let* ((rh (floor (log (1+ (random 15)) 2)))
               (r (+ (random 64) (* (random 64) (- 3 rh)))))
          (funcall putc (code-char r)))))))

(test char-noise-robustness
  ;; Processing 10KB random data should cause no Lisp errors.
  (is-true (ignore-errors (mex (rand-string 10240)) t)))

(defun rand-category-table ()
  (let ((base-cats (list mex::*ccat-invalid* mex::*ccat-whitespace*
                         mex::*ccat-newline* mex::*ccat-escape*
                         mex::*ccat-param* mex::*ccat-lbrace*
                         mex::*ccat-rbrace* mex::*ccat-letter*
                         mex::*ccat-number* mex::*ccat-other*))
        (flag-cats (list mex::*ccat-active* mex::*ccat-constituent*)))
    (loop for i :from -1 :below 256
          for rcat-base := (elt base-cats (random (length base-cats)))
          for rcat := (reduce
                        (lambda (flag cat) (logior (* flag (random 2)) cat))
                        flag-cats
                        :initial-value rcat-base)
          for ctab := mex:*plain-category-table*
            :then (char-cat i ctab rcat)
          finally (return ctab))))

(test char-noise-robustness-with-random-ccats
  ;; Same as CHAR-NOISE-ROBUSTNESS, but with character categories set at
  ;; random.
  (let ((*root-context* (mex::spawn-context *root-context*
                          :category-table (rand-category-table))))
    (is-true (ignore-errors (mex (rand-string 10240)) t))))

(let ((standard-command-tokens nil))
  (defun rand-standard-command-token ()
    (when (null standard-command-tokens)
      (setf standard-command-tokens
              (concatenate 'vector
                (loop for k :being each hash-key
                        :of (mex::command-table *root-context*)
                      collect k)
                (loop for k :being each hash-key
                        :of (gethash "" (mex::locale-table *root-context*))
                      unless (equal k "") collect k))))
    (aref standard-command-tokens
          (random (length standard-command-tokens)))))
