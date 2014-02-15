(in-package #:mex)


;;; Capacity guards

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *capacity-guards* '()
    "List of capacity guard variables.")

  (defvar *capacity-exceeded*
    (lambda (guard-name)
      (error "Mex ~(~A~) capacity exceeded" guard-name))
    "Function to call when capacity is exceeded.
     It must accept one argument, a symbol describing the capacity that was
     exceeded.  The default behvaiour is to signal an error.")

  (defpsvar *capacity-exceeded*
    (lambda (guard-name)
      (throw
        (new (*error (interpolate "Mex #{guard-name} capacity exceeded")))))    
    "Function to call when capacity is exceeded.
     It must accept one argument, a symbol describing the capacity that was
     exceeded.  The default behvaiour is to signal an error.")

  (defmacro+ps with-capacity-guards ((&rest bindings) &body body)
    "Run BODY with capacity guard variables bound locally."
    (let ((bindings+ (loop for capsym :in *capacity-guards*
                           unless (assoc capsym bindings)
                           collect (list capsym capsym))))
      `(let ,(append bindings bindings+)
         ,@body)))

  (defmacro+ps defguard (name max)
    "Define a capacity guard variable and a macro which performs some
     operation only after decreasing the capacity and checking that it has
     not been exceeded."
    (let ((capsym (intern (format nil "*~A-CAPACITY*" name)))
          (guardsym (intern (format nil "GUARDED-FOR-~A" name))))
      (pushnew capsym *capacity-guards*)
      `(progn
         (defvar ,capsym ,max)
         (defmacro ,guardsym (op &rest args)
           (let ((guard-name ',name) (capsym ',capsym))
             `(if (> (decf ,capsym) 0)
                  (,op ,@args)
                  (when *capacity-exceeded*
                    (funcall *capacity-exceeded* ',guard-name))))))))

  (defmacro+ps defstruct-guarded ((&whole name-and-options
                                   name &rest options)
                                  &rest slots)
    "Same as DEFSTRUCT, but additionally creates a guarded constructor,
     i. e. one which creates the given object only after decreasing the
     specified capacity and checking that it has not been exceeded."
    (let ((constr (intern (format nil "MAKE-~A" name)))
          (guard-name (cadr (assoc :guard options))))
      (if guard-name
          (let ((guarded (intern (format nil "GUARDED-FOR-~A" guard-name)))
                (guarded-make (intern (format nil "GUARDED-MAKE-~A" name)))
                (options+ (remove :guard options :key #'car)))
            `(progn
               (defstruct (,name ,@options+) ,@slots)
               (defmacro ,guarded-make (&rest args)
                 (list* ',guarded ',constr args))))
          `(defstruct ,name-and-options ,@slots))))

)


;;; Contexts

(ambi-ps ()

  (defguard context 100000)

  (defstruct-guarded (context (:conc-name) (:guard context))
    "A Mex processing context, holding local customizations of character
     categories, local bindings of commands and aliases, and register and
     stack storage."
    ;; A CAT-TABLE structure:
    category-table
    ;; A table mapping command keys (strings) to vectors of TOKENs:
    alias-table
    ;; A table mapping command keys (strings) to vectors of COMMANDs:
    command-table
    ;; A table mapping locale ids (strings) to command tables; "" in the
    ;; locale table maps to the current locale, and "" in every localized
    ;; command table maps to that locale's id:
    locale-table
    ;; A table mapping command keys (strings) to arbitrary values:
    register-table
    ;; A DOM-STACK structure:
    dom-stack
    ;; Contexts may be nested:
    parent-context)

  (defstruct-guarded (opaque-context (:include context) (:guard context))
    "A context whose category, command, and register tables may be
     modified while processing input.")

  (defvar *no-transparent-contexts* nil
    "May be temporarily set to true to allow the modification of contexts
     which otherwise are transparent (i. e. not opaque).")

  (defun ensure-opaque-context (context)
    "Return the CONTEXT or its nearest parent which is opaque."
    (if *no-transparent-contexts*
        context
        (loop for ctx := context :then (parent-context ctx)
              until (or (not ctx) (opaque-context-p ctx))
              finally (return ctx))))

  (defmacro spawn-context (&rest args)
    (let (context constructor slot-init-args)
      (destructure/or args
        ((ctx (guarded type) &rest kwargs)
         (when (eq guarded 'guarded)
           (setf context ctx slot-init-args kwargs
                 constructor (find-symbol
                               (format nil "GUARDED-MAKE-~A" type)))))
        ((ctx (type) &rest kwargs)
         (setf context ctx slot-init-args kwargs
               constructor (find-symbol (format nil "MAKE-~A" type))))
        ((ctx &rest kwargs)
         (when (or (null kwargs) (keywordp (car kwargs)))
           (setf context ctx slot-init-args kwargs
                 constructor 'make-context)))
        (t (error "Invalid use of macro: (SPAWN-CONTEXT~{ ~A~})"
                  args)))
      (with-var-value (context)
        `(,constructor
          ,@(loop for (kw val) :on slot-init-args :by #'cddr
                  with slot-init
                    = (list :category-table `(category-table ,context)
                            :dom-stack `(dom-stack ,context)
                            :parent-context context)
                  do (setf (getf slot-init kw) val)
                  finally (return slot-init))))))

  (defun set-context-locale (context locale-id)
    "Set the current locale (localized command table) in CONTEXT and in
     every ancestor of CONTEXT to the locale whose id is LOCALE-ID.
     Return non-NIL iff the current locale could be actually set in any
     context."
    (let* ((locales (locale-table context))
           (locale-command-table (lookup locale-id locales))
           (st (and locale-command-table
                    (remember "" locales locale-command-table)))
           (parent-st (and (parent-context context)
                           (set-context-locale (parent-context context)
                                               locale-id))))
      (or st parent-st)))

  (defun add-context-locale (context locale-id &optional proto)
    "Add a new entry to the locale table of CONTEXT, binding LOCALE-ID to a
     fresh localized command table.  If PROTO (a locale id) is supplied,
     the localized command table shall be a copy of that locale's command
     table; otherwise, it shall be an empty table."
    (let* ((locales (locale-table context))
           (proto-command-table (and proto (lookup proto locales))))
      (unless (lookup locale-id locales)
        (let ((locale-command-table
               (if proto
                   (and proto-command-table
                        (remember locale-id locales
                                  (copy-table proto-command-table)))
                   (remember locale-id locales (make-table)))))
          (when locale-command-table
            (remember "" locale-command-table locale-id)
            (remember "" locales locale-command-table))))))

  (defvar *default-locale* "en_US"
    "The lingua franca.")

  (defun make-locale-table (&optional (base-locale *default-locale*))
    "Create and return a locale table containing a single localized command
     table with locale id BASE-LOCALE."
    (let ((base-locale-command-table (make-table))
          (locale-table (make-table)))
      (remember "" base-locale-command-table base-locale)
      (remember base-locale locale-table base-locale-command-table)
      (remember "" locale-table base-locale-command-table)
      locale-table))

  (defun context-locale (context)
    "Return the locale id of CONTEXT's current locale."
    (loop for ctx := context :then (parent-context ctx)
          while ctx
          for locale-table := (locale-table context)
          for command-table := (and locale-table (lookup "" locale-table))
          for locale-id := (and command-table (lookup "" command-table))
          if locale-id return locale-id
          finally (return *default-locale*)))

)

(defmethod print-object ((ctx context) stream)
  (if *print-readably* (call-next-method)
      (print-unreadable-object (ctx stream :type t :identity t)
        (labels ((table-cmd-count (table)
                   (and (hash-table-p table)
                        (loop for cmds :being each hash-value in table
                                :using (hash-key key)
                              unless (equal key "")
                                summing (length cmds) :into count
                              finally (return (and (plusp count) count))))))
          (let ((cmd-count
                 (table-cmd-count (command-table ctx)))
                (lcmd-count
                 (and (hash-table-p (locale-table ctx))
                      (or (table-cmd-count
                            (gethash "" (locale-table ctx) nil))
                          (table-cmd-count
                            (gethash *default-locale* (locale-table ctx)
                                     nil))))) 
                (custom-cats
                 (and (category-table ctx)
                      (not (eq (category-table ctx)
                               *default-category-table*)))))
          (when (or cmd-count lcmd-count custom-cats)
            (format stream ":: ~@[~A cmds~]~@[~*, ~]~@[~A lcmds~]~
                               ~@[~*, ~]~@[~*cats+~]"
                    cmd-count (and cmd-count lcmd-count) lcmd-count
                    (and (or cmd-count lcmd-count) custom-cats)
                    custom-cats)))))))

;;; Local Variables: ***
;;; mode:lisp ***
;;; local-font-lock-keywords:(("(\\(def\\(?:\\(ps\\(?:macro\\|fun\\)\\|\\(?:un\\|macro\\)\\+ps\\)\\|\\(guard\\|enum\\)\\|\\(struct-guarded\\)\\)\\)[ \t\n]+(?\\([^()]*?\\)[ \t\n]" (1 font-lock-keyword-face) (5 (cond ((match-beginning 2) font-lock-function-name-face) ((match-beginning 3) font-lock-variable-name-face) ((match-beginning 4) font-lock-type-face)))) ("(\\(ambi-ps\\)\\_>" (1 font-lock-preprocessor-face))) ***
;;; End: ***
