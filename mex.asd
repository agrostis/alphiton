;;;; -*- lisp -*-

(defpackage #:mex-system
  (:use #:cl #:asdf))
(in-package #:mex-system)

(when (find-class 'selfward-operation nil)
  (pushnew :asdf-selfward-op *features*))

(defclass load-mex-prologue-op (#+asdf-selfward-op selfward-operation
                                #-asdf-selfward-op operation)
  ())

(defmethod output-files ((op load-mex-prologue-op) component)
  (declare (ignore component))
  (values nil nil))

(defun slurp-file (mex-file)
  (or (ignore-errors
        (with-open-file (in mex-file)
          (let ((str (make-array (file-length in)
                       :element-type 'character :fill-pointer t)))
            (setf (fill-pointer str) (read-sequence str in))
            str)))
      ""))

(defmethod perform ((op load-mex-prologue-op) component)
  (let ((mex-file (car (input-files op component)))
        (prologue-var (ignore-errors
                        (find-symbol "%PROLOGUE%" (find-package "MEX")))))
    (when (and prologue-var (boundp prologue-var))
      (set prologue-var (slurp-file mex-file)))))

(defmethod operation-done-p ((op load-mex-prologue-op) component)
  (declare (ignore component))
  nil)

(defun write-js (js-file)
  (let ((writer (ignore-errors
                  (symbol-function
                    (find-symbol "WRITE-JS" (find-package "MEX"))))))
    (when writer (funcall writer js-file))))

(defsystem #:mex
  :name "Mex"
  :author "Boris Smilga <boris.smilga@gmail.com>"
  :maintainer "Boris Smilga <boris.smilga@gmail.com>"
  :licence "BSD"
  :description "A macro processor for Web authoring" 
  :depends-on (#:parenscript #:cl-unicode #:cl-json)
  :components
    ((:static-file "mex.asd")
     (:module #:src
        :serial t
        :pathname ""
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
           (:static-file "prologue.mex")
           (:file "mex"
              :in-order-to ((compile-op
                             (load-mex-prologue-op "prologue.mex")))))
        :perform (compile-op :after (op src)
                   (write-js
                     (merge-pathnames "mex.js"
                                      (component-pathname src)))))))

(defun generate-tests (destination)
  (labels ((get-files (component)
             (typecase component
               ((or system module)
                (reduce #'append
                  (mapcar #'get-files (component-children component))))
               (cl-source-file
                (list (component-pathname component))))))
    (let ((generate-fn (ignore-errors
                         (find-symbol "GENERATE-TESTS"
                                      (find-package "MEX-TEST"))))
          (mex-files (get-files (find-system '#:mex))))
      (when (and generate-fn (fboundp generate-fn))
        (funcall generate-fn destination mex-files)))))

(defsystem #:mex.test
  :name "Mex.Test"
  :author "Boris Smilga <boris.smilga@gmail.com>"
  :maintainer "Boris Smilga <boris.smilga@gmail.com>"
  :licence "BSD"
  :depends-on (#:mex #:eos #:cl-json)
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
