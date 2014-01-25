;;;; -*- lisp -*-

(defpackage #:mex-system
  (:use #:cl #:asdf))
(in-package #:mex-system)

(defclass load-mex-prologue-op (operation)
  ())

(defmethod output-files ((op load-mex-prologue-op) component)
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
           ;(:file "memory")
           (:static-file "prologue.mex")
           (:file "mex"
              :in-order-to ((compile-op
                             (load-mex-prologue-op "prologue.mex")))))
        :perform (compile-op :after (op src)
                   (write-js
                     (merge-pathnames "mex.js"
                                      (component-pathname src)))))))
