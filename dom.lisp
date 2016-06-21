(in-package #:alphiton)

;; Equality of DOM objects

(defun dom-equal% (a b)
  "On Lisp side, this is like DOM-EQUAL, but with some special cases."
  (or (and (stringp a) (stringp b)
           (string= a b))
      (and (consp a) (consp b)
           (dom-equal% (car a) (car b))
           (dom-equal% (cdr a) (cdr b)))
      (dom-equal a b)))

(defpsfun dom-equal% (a b)
  "On JavaScript side, this is, effectively, an alias for DOM-EQUAL."
  (dom-equal a b))

(defun+ps dom-equal (a b)
  "Return true iff A and B are equal DOM structures."
  (or (and (dom-element-p a) (dom-element-p b)
           (equal (element-name a) (element-name b))
           (dom-equal (element-attributes a) (element-attributes b))
           (dom-equal% (element-content a) (element-content b)))
      (and (dom-text-p a) (dom-text-p b)
           (equal (text-content a) (text-content b)))
      (and (dom-comment-p a) (dom-comment-p b)
           (dom-equal% (comment-content a) (comment-content b)))
      (and (dom-recipe-p a) (dom-recipe-p b)
           (equal (recipe-handler-name a) (recipe-handler-name b))
           (dom-equal% (recipe-data a) (recipe-data b)))
      (and (vectorp a) (vectorp b)
           (vector-equal a b #'dom-equal%))
      (and (tablep a) (tablep b)
           (table-equal a b #'dom-equal%))
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


;; Lisp-side DOM to JSON to DOM

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


;; Lisp-side rendering

(defvar *render-recipes* (make-table)
  "Table mapping recipe names to functions invoked at render time.")

(defvar *render-stream* nil
  "Stream for rendering DOM trees.")

(let ((re (cl-ppcre:create-scanner "(&)|(<)|(>)|(\")|(--)")))
  (defun xml-quote (str &key ((:quot quot-p)) ((:hyphens hyphens-p)))
    (cl-ppcre:regex-replace-all re str
      (lambda (match amp lt gt quot hyphens)
        (cond (amp "&amp;") (lt "&lt;") (gt "&gt;")
              ((and quot quot-p) "&quot;")
              ((and hyphens hyphens-p) "&#x2d;&#x2d;")
              (t match)))
      :simple-calls t)))

(defgeneric render (dom &optional stream)
  (:documentation "Render DOM tree.")
  (:method :around (dom &optional (stream t stream-supplied-p))
    (let ((*render-stream*
           (case (or (not stream-supplied-p) stream)
             ((nil) (make-string-output-stream))
             ((t) *standard-output*)
             (t stream))))
      (call-next-method dom *render-stream*)
      (if (and stream-supplied-p (eq stream nil))
          (get-output-stream-string *render-stream*))))
  (:method ((elt dom-element) &optional stream)
    (format stream "<~A" (element-name elt))
    (when (element-attributes elt)
      (map-table
       (lambda (attr val)
         (format stream " ~A=\"~A\""
                 (ensure-string attr)
                 (xml-quote (ensure-string val) :quot t)))
       (element-attributes elt)))
    (if (zerop (length (element-content elt)))
        (format stream "/>")
        (progn
          (format stream ">")
          (loop for child :across (element-content elt)
                do (render child stream))
          (format stream "</~A>" (element-name elt)))))
  (:method ((text dom-text) &optional stream)
    (princ (xml-quote (text-content text)) stream))
  (:method ((comment dom-comment) &optional stream)
    (format stream "<!-- ~A -->"
            (xml-quote (comment-content comment) :hyphens t)))
  (:method ((recipe dom-recipe) &optional stream)
    (let ((recipe-handler
           (lookup (recipe-handler-name recipe) *render-recipes*)))
      (when recipe-handler
        (render (funcall recipe-handler (recipe-data recipe))
                stream))))
  (:method ((thing t) &optional stream)
    (princ (xml-quote (ensure-string thing)) stream)))


;; JS-side DOM to JSON to DOM

(ambi-ps (*js-target*)

  (defgeneric dom-to-json (dom)
    (:method ((elt dom-element))
      (create :e (element-name elt)
              :a (element-attributes elt)
              :p (dom-to-json (element-content elt))))
    (:method ((text dom-text))
      (create :t (text-content text)))
    (:method ((comment dom-comment))
      (create :c (comment-content comment)))
    (:method ((recipe dom-recipe))
      (create :r (recipe-handler-name recipe)
              :d (dom-to-json (recipe-data recipe))))
    (:method ((tok token))
      (input-to-string tok))
    (:method ((grp group))
      (input-to-string grp))
    (:method (thing)
      (cond ((vectorp thing)
             (loop for sub :across thing
                   collect (dom-to-json sub)))
            ((and thing (objectp thing))
             (let ((json (create)))
               (loop for (k v) :of thing
                     do (setf (getprop json k) v))))
            (t thing))))

  (defun json-to-dom (json)
    (cond
      ((vectorp json)
       (loop for sub :across json
             if (null sub)
               collect sub
             else
               do (setf sub (json-to-dom sub))
               and if (null sub) return nil
                   else collect sub))
      ((tablep json)
       (with-slots ((elt-name :e) (attrs :a) (content :p)
                    (text :t) (comment :c)
                    (recipe-handler :r) (recipe-data :d))
           json
         (cond
           ((and (stringp elt-name)
                 (and attrs (objectp attrs))
                 (loop for attr of attrs
                       when (and ((@ attrs has-own-property) attr)
                                 (not (stringp (getprop attrs attr))))
                         do (return nil)
                       finally (return true)))
            (let ((dom-content (json-to-dom content)))
              (when dom-content
                (make-dom-element :name elt-name :attributes attrs
                                  :content dom-content))))
           ((stringp text)
            (make-dom-text :content text))
           ((stringp comment)
            (make-dom-comment :content comment))
           ((stringp recipe-handler)
            (make-dom-recipe :handler-name recipe-handler
                             :data recipe-data)))))))

)

;; JS-side rendering

(ambi-ps (*js-target*)
  (defvar *render-recipes* (make-table)
    "Table mapping recipe names to functions invoked at render time.")

  (defgeneric render (dom document)
    (:documentation "Render DOM tree in the context of a document.")
    (:method ((elt dom-element) document)
      (let ((r-elt ((@ document create-element) (element-name elt))))
        (when (element-attributes elt)
          (map-table
            (lambda (attr val) ((@ r-elt set-attribute) attr val))
            (element-attributes elt)))
        (loop for child :across (element-content elt)
              for r-child := (render child document)
              if r-child do ((@ r-elt append-child) r-child))
        r-elt))
    (:method ((text dom-text) document)
      ((@ document create-text-node) (text-content text)))
    (:method ((comment dom-comment) document)
      ((@ document create-comment) (comment-content comment)))
    (:method ((recipe dom-recipe) document)
      (let ((recipe-handler
             (lookup (recipe-handler-name recipe) *render-recipes*)))
        (when recipe-handler
          (render (funcall recipe-handler (recipe-data recipe))
                  document))))
    (:method (thing document)
      (cond ((stringp thing)
             ((@ document create-text-node) thing))
            ((and (objectp thing) (instanceof thing *node))
             thing))))
)

;;; Local Variables: ***
;;; mode:lisp ***
;;; local-font-lock-keywords:(("(\\(def\\(?:\\(ps\\(?:macro\\|fun\\)\\|\\(?:un\\|macro\\)\\+ps\\)\\|\\(guard\\|enum\\)\\|\\(struct-guarded\\)\\)\\)[ \t\n]+(?\\([^()]*?\\)[ \t\n]" (1 font-lock-keyword-face) (5 (cond ((match-beginning 2) font-lock-function-name-face) ((match-beginning 3) font-lock-variable-name-face) ((match-beginning 4) font-lock-type-face)))) ("(\\(ambi-ps\\)\\_>" (1 font-lock-preprocessor-face))) ***
;;; End: ***
