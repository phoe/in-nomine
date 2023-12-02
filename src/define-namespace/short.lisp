;;;; This file is a part of IN-NOMINE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:in-nomine)

(defun %define-namespace-short-form
    (name &optional (value-type 't) (binding t) documentation)
  (check-name-not-in-cl-package name)
  (check-redefine-meta-namespace name)
  (let ((namespace (ensure-namespace name :value-type value-type :binding binding)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (ensure-namespace ',name :value-type ',value-type)
       ,@(make-type-forms namespace)
       ,@(make-proclamations namespace)
       ,@(make-binding-table-var-forms namespace)
       ,@(make-unbound-condition-forms namespace)
       ,@(make-reader-forms namespace)
       ,@(make-writer-forms namespace)
       ,@(make-let-forms namespace)
       ,@(make-boundp-forms namespace)
       ,@(make-makunbound-forms namespace)
       ,@(make-documentation-forms namespace documentation)
       ,@(make-documentation-table-var-forms namespace)
       (symbol-namespace ',name))))
