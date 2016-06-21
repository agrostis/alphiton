;;;; -*- lisp -*-

(defpackage #:alphiton-system
  (:use #:cl #:asdf))
(in-package #:alphiton-system)

(when (find-class 'selfward-operation nil)
  (pushnew :asdf-selfward-op *features*))

(defclass load-alphiton-prologue-op (#+asdf-selfward-op selfward-operation
                                     #-asdf-selfward-op operation)
  (#+asdf-selfward-op (selfward-operation :initform nil)))

(defmethod output-files ((op load-alphiton-prologue-op) component)
  (declare (ignore component))
  (values nil nil))

(defun slurp-file (alphiton-file)
  (or (ignore-errors
        (with-open-file (in alphiton-file)
          (let ((str (make-array (file-length in)
                       :element-type 'character :fill-pointer t)))
            (setf (fill-pointer str) (read-sequence str in))
            str)))
      ""))

(defmethod perform ((op load-alphiton-prologue-op) component)
  (let ((alphiton-file (car (input-files op component)))
        (prologue-var (ignore-errors
                        (find-symbol "%PROLOGUE%"
                                     (find-package "ALPHITON")))))
    (when (and prologue-var (boundp prologue-var))
      (set prologue-var (slurp-file alphiton-file)))))

(defmethod operation-done-p ((op load-alphiton-prologue-op) component)
  (declare (ignore component))
  nil)

(defun write-js (js-file)
  (let ((writer (ignore-errors
                  (symbol-function
                    (find-symbol "WRITE-JS%"
                                 (find-package "ALPHITON"))))))
    (when writer (funcall writer js-file))))

(defsystem #:alphiton
  :name "Alphiton"
  :author "Boris Smilga <boris.smilga@gmail.com>"
  :maintainer "Boris Smilga <boris.smilga@gmail.com>"
  :licence "BSD"
  :description "A macro processor for Web authoring"
  :depends-on (#:linewise-template #:parenscript
               #:cl-unicode #:cl-json #:cl-ppcre)
  :components
    ((:static-file "alphiton.asd")
     (:module #:patch-deps
        :serial t
        :pathname ""
        :components
          ((:file "patch-ps")))
     (:module #:src
        :serial t
        :pathname ""
        :depends-on (#:patch-deps)
        :components
          ((:file "package")
           (:file "ambi-ps")
           (:file "ccat")
           (:file "context")
           (:file "token")
           (:file "group")
           (:file "command")
           (:file "core")
           (:file "data")
           (:file "control")
           (:file "dom")
           (:static-file "prologue.thy")
           (:static-file "alphiton.js")
           (:file "alphiton"
              :in-order-to ((compile-op
                             (load-alphiton-prologue-op "prologue.thy")))))
        :perform (compile-op :after (op src)
                   (write-js
                     (merge-pathnames "alphiton.js"
                                      (component-pathname src)))))))

(defun generate-tests (destination
                       &optional (generate-fn-name '#:generate-tests))
  (labels ((get-files (component)
             (typecase component
               ((or system module)
                (reduce #'append
                  (mapcar #'get-files (component-children component))))
               (cl-source-file
                (list (component-pathname component))))))
    (let ((alphiton-files (get-files (find-system '#:alphiton)))
          (generate-fn (ignore-errors
                         (find-symbol (string generate-fn-name)
                                      (find-package "ALPHITON-TEST")))))
      (when (and generate-fn (fboundp generate-fn))
        (funcall generate-fn destination alphiton-files)))))

(defsystem #:alphiton.test
  :name "Alphiton.Test"
  :author "Boris Smilga <boris.smilga@gmail.com>"
  :maintainer "Boris Smilga <boris.smilga@gmail.com>"
  :licence "BSD"
  :depends-on (#:alphiton #:linewise-template #:eos #:cl-json)
  :components
    ((:module #:src
        :pathname ""
        :components ((:file "test-functions")))
     (:module #:generated-tests
        :pathname ""
        :depends-on (#:src)
        :components ((:file "generated-tests"))
        :perform (prepare-op :before (op src)
                   (generate-tests
                     (component-pathname
                       (car (component-children src))))))
     (:module #:manual-tests
        :pathname ""
        :depends-on (#:src)
        :components ((:file "environment-tests")))))
