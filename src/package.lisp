;;;; This file is a part of IN-NOMINE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(defpackage #:in-nomine
  (:use #:cl #:alexandria)
  (:local-nicknames (#:t #:trivial-arguments))
  (:export
   ;; Macros and utility functions
   #:define-namespace
   #:clear-namespace
   ;; Namespace structure accessors
   #:namespace
   #:namespace-name
   #:namespace-name-type
   #:namespace-value-type
   #:namespace-accessor
   #:namespace-condition-name
   #:namespace-type-name
   #:namespace-makunbound-symbol
   #:namespace-boundp-symbol
   #:namespace-documentation-type
   #:namespace-error-when-not-found-p
   #:namespace-errorp-arg-in-accessor-p
   #:namespace-default-arg-in-accessor-p
   #:namespace-hash-table-test
   #:namespace-binding-table
   #:namespace-documentation-table
   #:namespace-binding-table-var
   #:namespace-documentation-table-var
   #:namespace-definer-name
   #:namespace-definer
   ;; Metanamespace accessors
   #:namespace
   #:symbol-namespace
   #:namespace-boundp
   #:namespace-makunbound
   #:unbound-namespace))
