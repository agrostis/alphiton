(in-package #:alphiton-test)

(in-suite alphiton-tests)

(defmacro rand-string-test (name &body body)
  `(test ,name
     (macrolet ((trap-errors ((&rest plist) &rest forms)
                  (let ((name ',name))
                    `(multiple-value-bind (ret exc)
                         (ignore-errors
                           ,@(loop for (i var) on plist by #'cddr
                                   collect `(setf (get ',name ,i) ,var))
                           ,@forms)
                       (setf (get ',name :exception) exc)
                       ret))))
       ,@body)))

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

(rand-string-test char-noise-robustness
  ;; Processing 10KB random data should cause no Lisp errors.
  (is-true (let ((noise (rand-string 10240)))
             (trap-errors (:rand-string noise) (alphiton noise) t))))

(defun rand-category-table ()
  (let ((base-cats (list alphiton::*ccat-invalid* alphiton::*ccat-whitespace*
                         alphiton::*ccat-newline* alphiton::*ccat-escape*
                         alphiton::*ccat-param* alphiton::*ccat-lbrace*
                         alphiton::*ccat-rbrace* alphiton::*ccat-letter*
                         alphiton::*ccat-number* alphiton::*ccat-other*))
        (flag-cats (list alphiton::*ccat-active* alphiton::*ccat-constituent*)))
    (loop for i :from -1 :below 256
          for rcat-base := (elt base-cats (random (length base-cats)))
          for rcat := (reduce
                        (lambda (flag cat) (logior (* flag (random 2)) cat))
                        flag-cats
                        :initial-value rcat-base)
          for ctab := alphiton:*plain-category-table*
            :then (char-cat i ctab rcat)
          finally (return ctab))))

(rand-string-test char-noise-robustness-with-random-ccats
  ;; Same as CHAR-NOISE-ROBUSTNESS, but with character categories set at
  ;; random.
  (let ((rct (rand-category-table))
        (noise (rand-string 10240)))
    (let ((*root-context* (alphiton::spawn-context
                            (alphiton::init-root-context)
                            :category-table rct)))
      (is-true (trap-errors (:rand-string noise :rand-ccat rct)
                 (alphiton noise) t)))))

(let ((standard-command-tokens nil))
  (defun rand-standard-command-token ()
    (when (null standard-command-tokens)
      (alphiton::init-root-context)
      (setf standard-command-tokens
              (concatenate 'vector
                (loop for k :being each hash-key
                        :of (alphiton::command-table *root-context*)
                      collect k)
                (loop for k :being each hash-key
                        :of (gethash "" (alphiton::locale-table *root-context*))
                      unless (equal k "") collect k))))
    (aref standard-command-tokens
          (random (length standard-command-tokens)))))

(rand-string-test char-command-soup-robustness
  ;; Same as CHAR-NOISE-ROBUSTNESS, but with some characters in the string
  ;; replaced with commands from the standard set.
  (let ((soup (rand-string 10240
                (lambda (c)
                  (if (zerop (mod (random 1000) 4))
                      (rand-standard-command-token)
                      (string c))))))
    (is-true (trap-errors (:rand-string soup) (alphiton soup) t))))

(test simple-builtin
  (is-true
    (let ((plist nil))
      (alphiton "\\setp{foo}{one}\\setp{bar}{two}"
           (simple-builtins-table
             (setp (ind val)
               (setf (getf plist (intern (string-upcase ind) '#:keyword))
                     val)
               nil)))
      (and (equal (getf plist :foo) "one")
           (equal (getf plist :bar) "two")))))
