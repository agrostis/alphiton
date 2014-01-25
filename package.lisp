(defpackage #:mex
  (:use #:cl #:ps #:cl-unicode)
  (:export #:mex #:write-js))

(defvar mex::%prologue% ""
  "Code to run at start of Mex processing.
   This is normally loaded by ASDF.")
