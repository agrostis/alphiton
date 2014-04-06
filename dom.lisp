(in-package #:mex)

;; Equality of DOM objects

(defun dom-equal* (a b)
  "On Lisp side, this is like DOM-EQUAL, but with some special cases."
  (or (and (stringp a) (stringp b)
           (string= a b))
      (and (consp a) (consp b)
           (dom-equal* (car a) (car b))
           (dom-equal* (cdr a) (cdr b)))
      (dom-equal a b)))

(defpsfun dom-equal* (a b)
  "On JavaScript side, this is, effectively, an alias for DOM-EQUAL."
  (dom-equal a b))

(defun+ps dom-equal (a b)
  "Return true iff A and B are equal DOM structures."
  (or (and (dom-element-p a) (dom-element-p b)
           (equal (element-name a) (element-name b))
           (dom-equal (element-attributes a) (element-attributes b))
           (dom-equal* (element-content a) (element-content b)))
      (and (dom-text-p a) (dom-text-p b)
           (equal (text-content a) (text-content b)))
      (and (dom-comment-p a) (dom-comment-p b)
           (dom-equal* (comment-content a) (comment-content b)))
      (and (dom-recipe-p a) (dom-recipe-p b)
           (equal (recipe-handler-name a) (recipe-handler-name b))
           (dom-equal* (recipe-data a) (recipe-data b)))
      (and (tablep a) (tablep b)
           (table-equal a b #'dom-equal*))
      (and (vectorp a) (vectorp b)
           (vector-equal a b #'dom-equal*))
      (equal a b)))

(defun dom-p (thing)
  (or (dom-element-p thing) (dom-text-p thing) (dom-comment-p thing)
      (dom-recipe-p thing)))

(defvar *dom-print-recursive* nil)

(defmethod print-object ((element dom-element) stream)
  (if *print-readably* (call-next-method)
      (let ((id (ignore-errors
                   (let ((s (with-output-to-string (out)
                              (print-unreadable-object
                                  (element out :type nil :identity t)))))
                     (parse-integer s
                       :start (position-if #'alphanumericp s)
                       :radix 16 :junk-allowed t))))
            (name (element-name element))
            (attrs (element-attributes element))
            (content (element-content element)))
        (format stream "~:[#<DOM:~;<~]~:@(~A~)~@[ lisp:id='~X'~]"
                *dom-print-recursive* name id)
        (when attrs
          (maphash (lambda (name val)
                     (format stream " ~(~A~)='~A'" name val))
                   attrs))
        (if content
            (progn
              (princ ">" stream)
              (let ((*dom-print-recursive* t))
                (map nil
                  (lambda (sub) (when sub (write sub :stream stream)))
                  content))
              (format stream "</~:[DOM:~;~]~:@(~A~)>"
                      *dom-print-recursive* name))
            (princ "/>" stream)))))

(defmethod print-object ((text dom-text) stream)
  (if *print-readably* (call-next-method)
      (if *dom-print-recursive*
          (loop for c :across (text-content text)
                do (princ (case c
                            (#\< "&lt;") (#\> "&gt;") (#\& "&amp;") (t c))
                          stream))
          (format stream "#<[CDATA[~A]]>" (text-content text)))))

(defmethod print-object ((comment dom-comment) stream)
  (if *print-readably* (call-next-method)
      (let ((content (comment-content comment)))
        (format stream "~:[#~;~]<!-- " *dom-print-recursive*)
        (let ((*dom-print-recursive* t))
          (write
            (if (dom-p content) content
                (make-dom-text :content (princ-to-string content)))
            :stream stream))
        (princ " -->" stream))))

(defmethod print-object ((recipe dom-recipe) stream)
  (if *print-readably* (call-next-method)
      (let ((hname (recipe-handler-name recipe))
            (data (recipe-data recipe)))
        (format stream "~:[#~;~]<? DOM:RECIPE ~A "
                *dom-print-recursive* hname)
        (let ((*dom-print-recursive* nil)
              (*print-pretty* nil))
          (write data :stream stream))
        (princ " ?>" stream))))


;; DOM to JSON to DOM

(defun+ps dom-stack-get-root (stacks)
  "Close all open elements in the given multi-stack, and return the root
   element."
  (loop for elt := (dom-stack-close-element stacks)
        until (stack-empty-p (element-stack stacks))
        finally (return elt)))

(defgeneric dom-to-json (dom)
  (:documentation "Encode DOM tree as JSON.")
  (:method ((elt dom-element))
    (json:with-object ()
      (json:encode-object-member :e (element-name elt))
      (json:encode-object-member :a (element-attributes elt))
      (json:as-object-member (:p)       ; P is for payload
        (json:with-array ()
          (loop for child :across (element-content elt)
                do (json:as-array-member () (dom-to-json child)))))))
  (:method ((text dom-text))
    (json:with-object ()
      (json:encode-object-member :t (text-content text))))
  (:method ((comment dom-comment))
    (json:with-object ()
      (json:encode-object-member :c (comment-content comment))))
  (:method ((recipe dom-recipe))
    (json:with-object ()
      (json:encode-object-member :r (recipe-handler-name recipe))
      (json:as-object-member (:d)
        (handler-bind
            ((json:unencodable-value-error
               (lambda (err)
                 (let ((datum (type-error-datum err)))
                   (if (or (token-p datum) (group-p datum))
                       (json:encode-json (input-to-string datum))
                       (dom-to-json datum))))))
          (json:encode-json (recipe-data recipe))))))
  (:method ((thing t))
    (json:encode-json nil)))
        
(defun json-to-dom (json)
  (let ((dom-accumulator nil))
    (declare (special dom-accumulator))
    (labels ((get-dom-accumulator (keysym)
               (case keysym
                 ((:e :p :a)
                  (cond
                    ((dom-element-p dom-accumulator) dom-accumulator)
                    ((not dom-accumulator) (make-dom-element))))
                 ((:t)
                  (and (not dom-accumulator) (make-dom-text)))
                 ((:c)
                  (and (not dom-accumulator) (make-dom-comment)))
                 ((:r :d)
                  (cond
                    ((dom-recipe-p dom-accumulator) dom-accumulator)
                    ((not dom-accumulator) (make-dom-recipe))))))
             (ensure-table (value)
               (ignore-errors
                 (loop for (k . v) :in value
                       always (and (keywordp k) (stringp v))
                       with table := (make-table)
                       do (remember (string-downcase (symbol-name k)) table
                                    v)
                       finally (return table))))
             (dom-set (keysym value)
               (when (and (typep dom-accumulator 'structure-object)
                          (case keysym
                            ((:e :t :c :r) (stringp value))
                            ((:a) (setf value (ensure-table value)))
                            ((:p) (vectorp value))
                            ((:d) t)))
                 (case keysym
                   (:e (setf (element-name dom-accumulator) value))
                   (:a (setf (element-attributes dom-accumulator) value))
                   (:p (setf (element-content dom-accumulator) value))
                   (:t (setf (text-content dom-accumulator) value))
                   (:c (setf (comment-content dom-accumulator) value))
                   (:r (setf (recipe-handler-name dom-accumulator) value))
                   (:d (setf (recipe-data dom-accumulator) value)))
                 t)))
      (json:with-decoder-simple-list-semantics
        (let ((begh json:*beginning-of-object-handler*)
              (keyh json:*object-key-handler*)
              (valh json:*object-value-handler*)
              (endh json:*end-of-object-handler*))
          (json:set-custom-vars
            :array-type
              'vector
            :object-scope
              '(dom-accumulator)
            :beginning-of-object
              (lambda ()
                (setf dom-accumulator nil
                      json:*object-value-handler* valh)
                (funcall begh))
            :object-key
              (lambda (key)
                (let* ((keysym
                        (find-symbol
                          (funcall json:*json-identifier-name-to-lisp* key)
                          '#:keyword))
                       (target
                        (get-dom-accumulator keysym)))
                  (if target
                      (setf dom-accumulator
                              target
                            json:*object-value-handler*
                              (lambda (value)
                                (unless (dom-set keysym value)
                                  (setf dom-accumulator :fail))
                                (funcall valh value)))
                      (setf dom-accumulator :fail
                            json:*object-value-handler* valh)))
                (funcall keyh key))
            :end-of-object 
              (lambda ()
                (if (typep dom-accumulator 'structure-object)
                    dom-accumulator
                    (funcall endh))))
          (json:decode-json-from-source json))))))

;;; Local Variables: ***
;;; mode:lisp ***
;;; local-font-lock-keywords:(("(\\(def\\(?:\\(ps\\(?:macro\\|fun\\)\\|\\(?:un\\|macro\\)\\+ps\\)\\|\\(guard\\|enum\\)\\|\\(struct-guarded\\)\\)\\)[ \t\n]+(?\\([^()]*?\\)[ \t\n]" (1 font-lock-keyword-face) (5 (cond ((match-beginning 2) font-lock-function-name-face) ((match-beginning 3) font-lock-variable-name-face) ((match-beginning 4) font-lock-type-face)))) ("(\\(ambi-ps\\)\\_>" (1 font-lock-preprocessor-face))) ***
;;; End: ***
