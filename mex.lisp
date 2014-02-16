(in-package #:mex)

;;; The entry point

(ambi-ps ()

  (defvar *root-context* nil)

  (defvar *prologue* #.%prologue%
    "Mex code to run at start of processing.")

  (defun init-root-context ()
    (or *root-context*
        (setf *root-context*
                (with-capacity-guards ()
                  (let ((ctx (make-context
                               :category-table *category-table*
                               :command-table *command-table*
                               :locale-table (make-locale-table)))
                        (tsrc (make-token-source
                                :char-source *prologue*
                                :char-source-offset 0))
                        (*no-transparent-contexts* t))
                    (get-group-contents tsrc ctx t nil) 
                    ctx)))))

  (defun mex (source &optional want-context)
    (init-root-context)
    (with-capacity-guards ()
      (let* ((ctx (if (context-p want-context)
                      want-context
                      (guarded-make-opaque-context
                        :category-table *category-table*
                        :parent-context *root-context*)))
             (tsrc (make-token-source
                     :char-source source
                     :char-source-offset 0))
             #+nil (previous-shipped nil)
             #+nil (previous-shipped-count 0) 
             (ship (unless want-context
                     (lambda (output ctx)
;                      (format *trace-output*
;                              "~&******** Shipping ~S to ~S ********~%"
;                              output ctx)
                       (when (or (error-display-p output)
                                 (token-p output))
                         (context-stack-push output ctx))))
               #+nil (lambda (output ctx)
                     (declare (ignore ctx))
                     (if (eq output previous-shipped)
                         (when (> (incf previous-shipped-count) 10)
                           (error "Loop detected, aborting."))
                         (setf previous-shipped output
                               previous-shipped-count 0))
                     (format *trace-output*
                             "~&******** Shipping ~S ********~%"
                             output))))
        (parser-state-bind (:error eot)
            (get-group-contents tsrc ctx t ship)
          (when (error-display-p eot)
            (funcall ship eot ctx)))
        (if want-context
            ctx
            (with-context-dom-stack (stacks ctx)
              (dom-stack-get-root stacks))))))

)
