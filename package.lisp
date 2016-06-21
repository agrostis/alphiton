(in-package #:cl-user)

(defpackage #:alphiton
  (:use #:cl #:ps #:cl-unicode)
  (:export #:alphiton #:write-js%
           ;; Category tables
           #:*category-table* #:char-cat #:cat-table-from-unicode-blocks%
           #:*default-category-table* #:*plain-category-table*
           ;; Contexts
           #:*capacity-exceeded* #:*default-locale* #:*root-context*
           #:*prologue*
           ;; Tokens and groups
           #:tokens% #:data-to-input
           ;; Simple builtins
           #:define-simple-builtin #:simple-builtins-table
           ;; DOM construction
           #:make-dom-element #:element-name #:element-content
           #:make-dom-attribute #:attribute-name #:attribute-value
           #:make-dom-text #:text-content
           #:make-dom-comment #:comment-content
           #:make-dom-recipe #:recipe-handler-name #:recipe-data
           #:element #:text-node #:recipe
           #:*dom-root-wrapper* #:*dom-error-wrapper*
           #:dom-to-json #:json-to-dom #:dom-equal #:dom-equal%
           #:*render-recipes* #:render))

(defvar alphiton::%prologue% ""
  "Code to run at start of Alphiton processing.
   This is normally loaded by ASDF.")
