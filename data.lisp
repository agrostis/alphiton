(in-package #:mex)

;;; Document stack and data registers

(ambi-ps ()

  ;;; Stack

  (defstruct (dom-stacks (:conc-name))
    "The DOM multi-stack.  ELEMENT-STACK represents a tower of nested open
     DOM-ELEMENTS.  DOM-ELEMENT, DOM-TEXT and DOM-COMMENT objects placed on
     CURRENT-STACK become successive child nodes of the innermost open
     DOM-ELEMENT.  TOKENs are packed to DOM-TEXT objects which are handled
     in the general manner.  DOM-ATTRIBUTE objects become the element's
     attributes, all subject to verification.  DOM-RECIPE objects may be
     used to invoke dynamic behviour on the client side.  Objects of other
     types may be placed on the stack but are discarded when the ."
     current-stack element-stack id-counter)

  (defstruct (dom-element (:conc-name element-))
    "A DOM element.  CONTENT is a stack which may replace CURRENT-STACK in a
     DOM-STACKS object."
    name attributes content)

  (defstruct (dom-text (:conc-name text-))
    "A DOM text node.  CONTENT is a string."
    content)

  (defstruct (dom-comment (:conc-name comment-))
    "A DOM comment node."
    content)

  (defstruct (dom-recipe (:conc-name recipe-))
    "An object which should invoke some dynamic behaviour identified by
     HANDLER-NAME, acting on DATA, when the DOM structure is rendered
     client-side."
    handler-name data)

  (defmacro element (name &rest content)
    "A more concise DOM-ELEMENT constructor syntax."
    (loop for content :on content :by #'cddr
          for (attr val) := content
          while (keywordp attr)
          collect (string-downcase (symbol-name attr)) :into attrs
          collect val :into attrs
          finally (return
                    `(make-dom-element
                       :name ,name
                       :attributes (make-table ,@attrs)
                       :content (vector ,@content)))))

  (defvar *dom-root-wrapper*
    (lambda (&optional content)
      (let ((elt (element "DIV" :class "mex_wrapper")))
        (setf (element-content elt) (ensure-vector content))
        elt))
    "The value is a function returning a DOM-ELEMENT that Mex should put at
     the root of the DOM multi-stack.  The function should accept one
     optional argument (an input or vector of inputs), but may ignore it.")

  (defun init-dom-stack (context)
    (loop for ctx := context :then (parent-context ctx)
          while ctx
          for stacks = (dom-stack ctx)
          until stacks
          with gctx := nil
          with rctx := nil
          if (opaque-context-p ctx) do (setf gctx ctx)
          do (setf rctx ctx)
          finally (unless stacks
                    (let ((relt (funcall *dom-root-wrapper*)))
                      (setf stacks
                              (make-dom-stacks
                                :current-stack (copy-stack
                                                 (element-content relt))
                                :element-stack (stack relt)
                                :id-counter 0)))
                    (unless *no-transparent-contexts*
                      (setf rctx gctx))
                    (when (and rctx (not (eq rctx context)))
                      (setf (dom-stack rctx) stacks)))
                  (return stacks)))

  (defmacro with-context-dom-stack ((var context &key elements current id)
                                    &body body)
    "Evaluate BODY with VAR bound to the DOM multistack associated with
     CONTEXT. If necessary, initialize the stack by opening a root DIV."
    `(bind/init ((,var (dom-stack ,context) (init-dom-stack ,context)))
       ,@(if (or elements current id)
             `((let (,@(if elements `((,elements (element-stack ,var))))
                     ,@(if current `((,current (current-stack ,var))))
                     ,@(if id `((,id (id-counter ,var)))))
                 (prog1 (progn ,@body)
                   (setf ,@(if elements `((element-stack ,var) ,elements))
                         ,@(if current `((current-stack ,var) ,current))
                         ,@(if id `((id-counter ,var) ,id))))))
             body)))

  (defun text-node (input)
    "A more concise DOM-TEXT constructor syntax."
    (make-dom-text
      :content (if (stringp input) input (input-to-string input))))

  (defun recipe (handler data)
    "A more concise DOM-RECIPE constructor syntax."
    (make-dom-recipe :handler-name handler :data data))

  (defun dom-stack-next-id (stacks &optional (prefix "Mex"))
    "Generate and return a new id which is guaranteed to differ from all ids
     previously generated for STACKS."
    (bind/init ((n (id-counter stacks) 1))
      (prog1 (interpolate "#{prefix}#{n}")
        (setf (id-counter stacks) (1+ n)))))

  (defun dom-stack-open-element (element stacks)
    "Push a new open element ELT onto the element stack in STACKS.  If ELT
     has some content, copy it onto the current stack, otherwise, leave the
     current stack empty."
    (let ((next-element (stack-peek (element-stack stacks))))
      (when next-element
        (setf (element-content next-element)
                (ensure-vector (current-stack stacks)))))
    (setf (current-stack stacks)
            (if (element-content element)
                (copy-stack (element-content element))
                (make-stack)))
    (stack-push element (element-stack stacks))
    element)

  (defun dom-stack-close-element (stacks)
    "Close, pop, and return the most recently open element on STACKS.
     TOKENs placed on the current stack are packed to DOM-TEXT; these and
     other DOM objects are then verified and replace the content of the
     element.  If verification fails, an ERROR-DISPLAY is returned instead
     of the closed object."
    (let ((elt-stack (element-stack stacks)))
      (unless (stack-empty-p elt-stack)
        (let* ((closed-element (stack-pop elt-stack))
               (next-element (stack-peek elt-stack))
               (content (verify-element-content
                          (ensure-vector (current-stack stacks))
                          closed-element stacks))
               (ret-element (if (error-display-p content)
                                content
                                (progn
                                  (setf (element-content closed-element)
                                          content)
                                  closed-element))))
          (when next-element
            (stack-push
              ret-element
              (setf (current-stack stacks)
                    (copy-stack (element-content next-element)))))
          ret-element))))

  (defun verify-element-content (content parent-element stacks)
    "Ensure that CONTENT is valid as the content of PARENT-ELEMENT, that is:
      * merge all consecutive runs of tokens into DOM-TEXT nodes;
      * convert error displays to their DOM-ELEMENT representations; 
      * remove everything else which is not DOM stuff.
     Return validated content as a vector of DOM-ELEMENT, DOM-TEXT,
     DOM-COMMENT and/or DOM-RECIPE data."
    (declare (ignore parent-element))
    (let ((validated (make-stack)))
      (loop for i :from 0 :to (length content)
            for elt := (aref* content i)
            with text-node := nil
            if (token-p elt)
              if (not text-node)
                do (setf text-node (make-dom-text :content (make-stack)))
                end
              and do (stack-push (input-to-string elt)
                                 (text-content text-node))
            else
              if text-node
                do (setf (text-content text-node)
                           (ensure-string (text-content text-node)))
                   (stack-push text-node validated)
                   (setf text-node nil)
                end
              and if (or (dom-element-p elt)
                         (dom-comment-p elt)
                         (dom-text-p elt)
                         (dom-recipe-p elt))
                do (stack-push elt validated)
              else if (error-display-p elt)
                do (let ((id (dom-stack-next-id stacks "MexError")))
                     (stack-push (error-display-to-dom elt id)
                                 validated)))
      #| TBD: verification according to document type |#
      validated))

  (defvar *dom-error-wrapper*
    (lambda (input eotp messages id)
      (element "SPAN"
        :class "mex_error_display" :id id
        (text-node input)
        (when eotp (element "SPAN" :class "mex_eot"))
        (recipe "mex_error_display_attach_messages"
          (js-quote ((:id . (js-unquote id))
                     (:messages . (js-unquote messages)))))))
    "The value is a function returning a DOM-ELEMENT that Mex should
     substitute for an ERROR-DISPLAY.  The function should accept four
     arguments: a string representing the faulty input, a boolean indicating
     whether the faulty input included an end of text, a sequence of strings
     representing the error messages, and an id for the wrapper element.")

  (defun error-display-to-dom (error-display id)
    "Return a DOM-ELEMENT representing the error encoded by ERROR-DISPLAY."
    (declare (special *root-context*))
    (loop
      for msg :across (error-display-message error-display)
      with str with error with errors
      do (if (stringp msg)
             (setf str msg error nil)
             (let ((msg-tsrc (simulate-command-with-tokens
                               "errmsg" msg nil *root-context* nil)))
               (if msg-tsrc
                   (let ((pst (get-group-string msg-tsrc *root-context*)))
                     (setf str (parser-value pst)
                           error (parser-error pst)))
                   (setf str nil error t))))
      if error
        do (setf errors t)
      else
        collect str :into msgs
      finally
        (let ((msgs0 (ensure-vector msgs))
              (faulty-input (error-display-faulty-input error-display)))
          (return
            (funcall *dom-error-wrapper*
              (input-to-string faulty-input)
              (loop for i :from (1- (length faulty-input)) :downto 0
                    if (eot-p (aref faulty-input i)) return t)
              (if errors
                  (if (= (length msgs0) 0)
                      (vector "Internal error while displaying errors")
                      (spliced msgs0 (length msgs0) 0
                        #.(concatenate 'string
                            "Additionally, some errors could not be"
                            " displayed due to internal error")))
                  msgs0)
              id)))))

  (defmacro element-attr-bind ((&rest bindings) element &body body)
    (with-ps-gensyms (attrs)
      (flet ((attr-name (x) (string-downcase (string x))))
        `(let* ((,attrs (element-attributes ,element))
                ,@(loop
                    for binding :in bindings
                    for (var . name)
                      := (if (symbolp binding)
                             (cons binding (attr-name binding))
                             (destructuring-bind (var &optional nm) binding
                               (cons var (or nm (attr-name var)))))
                    collect `(,var (lookup ,name ,attrs))))
           ,@body))))

  (defmethod data-to-input ((element dom-element) context)
    (element-attr-bind (id class) element
      (let ((elt-name (element-name element))
            (hash-id (if id (interpolate "###{id}") ""))
            (dot-class (if class (interpolate ".#{class}") "")))
        (string-to-input (interpolate "<#{elt-name}#{hash-id}#{dot-class}/>")
                         context *plain-category-table*))))

  (defmethod data-to-input ((text-node dom-text) context)
    (string-to-input (text-content text-node)
                     context *plain-category-table*))

  (defmethod data-to-input ((comment dom-comment) context)
    (let ((content (comment-content comment)))
      (string-to-input (interpolate "<!-- #{content} --/>")
                       context *plain-category-table*)))

  (defmethod data-to-input ((recipe dom-recipe) context)
    (let ((name (recipe-handler-name recipe))
          (data-json (json:encode-json-to-string (recipe-data recipe))))
      (string-to-input (interpolate "<? recipe #{name} #{data-json} ?>")
                       context *plain-category-table*)))

  (defun data-to-input* (data context)
    "Like DATA-TO-INPUT, but with support for vectors and booleans."
    (if (vectorp data)
        (loop for sub :across data
              for input := (data-to-input* sub context)
              for inputs := (ensure-vector input)
                :then (if (vectorp input)
                          (vector-add inputs input)
                          (spliced inputs (length inputs) 0 input))
              finally (return inputs))
        (data-to-input
          (cond ((true-p data) "true") ((false-p data) "false") (t data))
          context)))

)

;;; Local Variables: ***
;;; mode:lisp ***
;;; local-font-lock-keywords:(("(\\(def\\(?:\\(ps\\(?:macro\\|fun\\)\\|\\(?:un\\|macro\\)\\+ps\\|builtin\\(?:-iterator\\)?\\)\\|\\(guard\\|enum\\)\\|\\(struct-guarded\\)\\)\\)[ \t\n]+(?\\([^()]*?\\)[ \t\n]" (1 font-lock-keyword-face) (5 (cond ((match-beginning 2) font-lock-function-name-face) ((match-beginning 3) font-lock-variable-name-face) ((match-beginning 4) font-lock-type-face)))) ("(\\(ambi-ps\\)\\_>" (1 font-lock-preprocessor-face))) ***
;;; End: ***
