;;;; This file is a part of IN-NOMINE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:in-nomine)

;;; Namespace structure and constructor

;;; Null ACCESSOR: no autogenerated accessors
;;; Null MAKUNBOUND-SYMBOL: no makunbound is generated
;;; Null BOUNDP-SYMBOL: no boundp is generated
;;; Null ACCESSOR, MAKUNBOUND-SYMBOL, and BOUNDP-SYMBOL: no binding hash table
;;;
;;; Null TYPE-NAME: no type is generated
;;; Null DOCUMENTATION-TYPE: no documentation hash table
;;; * Documentation of type T for namespace objects is always available
;;;
;;; Null CONDITION-NAME: no condition is generated, and:
;;; * True ERRORP-ARG-IN-ACCESSOR-P:
;;;  * compile-time-error
;;; * True ERROR-WHEN-NOT-FOUND-P:
;;;   * compile-time error
;;; * Null ERROR-WHEN-NOT-FOUND-P and ERRORP-ARG-IN-ACCESSOR-P:
;;;  * no error/restart facility in reader function

(macrolet ((e () '(error "Internal error - not all args were provided.")))
  (defstruct (namespace (:constructor %make-namespace))
    (name                      (e) :type symbol  :read-only t)
    (name-type                 (e) :type t       :read-only t)
    (value-type                (e) :type t       :read-only t)
    (accessor                  (e) :type symbol  :read-only t)
    (condition-name            (e) :type symbol  :read-only t)
    (type-name                 (e) :type symbol  :read-only t)
    (makunbound-symbol         (e) :type symbol  :read-only t)
    (boundp-symbol             (e) :type symbol  :read-only t)
    (documentation-type        (e) :type symbol  :read-only t)
    (hash-table-test           (e) :type symbol  :read-only t)
    (error-when-not-found-p    (e) :type boolean :read-only t)
    (errorp-arg-in-accessor-p  (e) :type boolean :read-only t)
    (default-arg-in-accessor-p (e) :type boolean :read-only t)
    (documentation             (e) :type (or null string))
    (binding-table             (e) :type (or null hash-table))
    (documentation-table       (e) :type (or null hash-table))
    (binding-table-var         (e) :type symbol  :read-only t)
    (definer-name              (e) :type symbol  :read-only t)
    (definer                   (e) :type t       :read-only t)
    (documentation-table-var   (e) :type symbol  :read-only t)))

(defun check-namespace-parameters (namespace)
  (when (null (namespace-condition-name namespace))
    (when (namespace-error-when-not-found-p namespace)
      (error "Cannot provide ERROR-WHEN-NOT-FOUND-P when CONDITION-NAME ~
              is null."))
    (when (namespace-errorp-arg-in-accessor-p namespace)
      (error "Cannot provide ERRORP-ARG-IN-ACCESSOR-P when CONDITION-NAME ~
              is null."))))

(defun check-namespace-definer-spec (definer)
  (or (symbolp definer)
      (when (listp definer)
        (case (first definer)
          (function t)
          (quote (and (symbolp (second definer))
                      (not (cddr definer))))
          (lambda (listp (second definer)))
          (t (listp (first definer)))))
      (error "Malformed definer ~S"
             definer)))

(defun make-namespace
    (name &key
            (name-type 'symbol)
            (value-type 't)
            (accessor (symbolicate '#:symbol- name))
            (condition-name (symbolicate '#:unbound- name))
            (type-name (symbolicate name '#:-type))
            (makunbound-symbol (symbolicate name '#:-makunbound))
            (boundp-symbol (symbolicate name '#:-boundp))
            (documentation-type nil documentation-type-p)
            (hash-table-test 'eq)
            (error-when-not-found-p t)
            (errorp-arg-in-accessor-p nil)
            (default-arg-in-accessor-p t)
            (binding-table-var nil)
            (definer-name nil)
            (definer nil)
            (documentation-table-var nil)
            (documentation nil))
  (check-namespace-definer-spec definer)
  (let* ((definer-name (or definer-name
                           (and definer (symbolicate '#:define- name))))
         (namespace (%make-namespace
                     :name name :name-type name-type :value-type value-type
                     :accessor accessor
                     :condition-name condition-name :type-name type-name
                     :makunbound-symbol makunbound-symbol
                     :boundp-symbol boundp-symbol
                     :error-when-not-found-p error-when-not-found-p
                     :errorp-arg-in-accessor-p errorp-arg-in-accessor-p
                     :default-arg-in-accessor-p default-arg-in-accessor-p
                     :documentation-type (if documentation-type-p
                                             documentation-type
                                             name)
                     :hash-table-test hash-table-test
                     :binding-table
                     (if (and (null accessor)
                              (null makunbound-symbol)
                              (null boundp-symbol))
                         nil
                         (make-hash-table :test hash-table-test))
                     :documentation-table
                     (if (and documentation-type-p (null documentation-type))
                         nil
                         (make-hash-table :test hash-table-test))
                     :binding-table-var binding-table-var
                     :definer-name definer-name
                     :definer (or definer (and definer-name t))
                     :documentation-table-var documentation-table-var
                     :documentation documentation)))
    (check-namespace-parameters namespace)
    namespace))

;;; Instantiating the metanamespace

(defparameter *namespace-args*
  '(:value-type namespace
    :documentation-type nil
    :default-arg-in-accessor-p nil
    :errorp-arg-in-accessor-p t
    :type-name nil
    :binding-table-var nil
    :documentation-table-var nil))

(defvar *namespaces* (apply #'make-namespace 'namespace *namespace-args*))

;;; Tying the knot

(setf (gethash 'namespace (namespace-binding-table *namespaces*)) *namespaces*)

;;; Helper functions

(defun ensure-namespace (name &rest args)
  (let ((hash-table (namespace-binding-table *namespaces*)))
    (multiple-value-bind (value foundp) (gethash name hash-table)
      (if foundp
          value
          (setf (gethash name hash-table)
                (apply #'make-namespace name args))))))
