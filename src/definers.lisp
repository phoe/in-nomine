;;;; This file is a part of IN-NOMINE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:in-nomine)

;;; Minor forms

(defun make-proclamations (namespace)
  (let* ((name (namespace-name namespace))
         (name-type (namespace-name-type namespace))
         (accessor (namespace-accessor namespace))
         (boundp (namespace-boundp-symbol namespace))
         (makunbound (namespace-makunbound-symbol namespace))
         (type (or (namespace-type-name namespace) 't))
         (errorp-arg-p (namespace-errorp-arg-in-accessor-p namespace))
         (default-arg-p (namespace-default-arg-in-accessor-p namespace))
         (error-when-not-found-p (namespace-error-when-not-found-p namespace)))
    `((declaim
       ,@(when accessor
           `((ftype (function (,name-type
                               &optional
                               ,@(when errorp-arg-p `(t))
                               ,@(when default-arg-p `((or ,type null))))
                              (values ,(if (and error-when-not-found-p
                                                (not errorp-arg-p))
                                           type
                                           `(or ,type null))
                                      &optional))
                    ,accessor)
             (inline ,accessor)))
       ;; We do not generate a writer for namespace NAMESPACE.
       ,@(when (and accessor (not (eq name 'namespace)))
           `((ftype (function (,type ,name-type &optional
                                     ,@(when errorp-arg-p `(t))
                                     ,@(when default-arg-p
                                         `((or ,type null))))
                              (values ,type &optional))
                    (setf ,accessor))
             (inline (setf ,accessor))))
       ,@(when boundp
           `((ftype (function (,name-type) (values boolean &optional)) ,boundp)))
       ,@(when makunbound
           `((ftype (function (,name-type)
                              (values ,name-type &optional))
                    ,makunbound)))))))

(defun make-unbound-condition-forms (namespace)
  (let ((name (namespace-name namespace))
        (condition (namespace-condition-name namespace)))
    (when condition
      `((define-condition ,condition (cell-error) ()
          (:report (lambda (condition stream)
                     (format stream "Name ~S is unbound in namespace ~S."
                             (cell-error-name condition) ',name))))))))

(defun make-type-forms (namespace)
  (let ((type-name (namespace-type-name namespace))
        (value-type (namespace-value-type namespace)))
    (when type-name
      `((deftype ,type-name () ',value-type)))))

(defun make-boundp-forms (namespace)
  (let ((name (namespace-name namespace))
        (table-symbol (namespace-binding-table-var namespace))
        (boundp (namespace-boundp-symbol namespace)))
    (when boundp
      `((defun ,boundp (name)
          "Automatically defined boundp function."
          ,@(when table-symbol `((declare (special ,table-symbol))))
          (let* ((hash-table ,(or table-symbol
                                  `(namespace-binding-table
                                    (symbol-namespace ',name)))))
            (nth-value 1 (gethash name hash-table))))))))

(defun make-makunbound-forms (namespace)
  (let ((name (namespace-name namespace))
        (table-symbol (namespace-binding-table-var namespace))
        (makunbound (namespace-makunbound-symbol namespace)))
    (when makunbound
      `((defun ,makunbound (name)
          "Automatically defined makunbound function."
          ,@(when table-symbol `((declare (special ,table-symbol))))
          (,@(if (eq name 'namespace)
                 `(if (eq name 'namespace)
                      (error "Unable to remove the NAMESPACE namespace."))
                 `(progn))
           (let* ((hash-table ,(or table-symbol
                                   `(namespace-binding-table
                                     (symbol-namespace ',name)))))
             (remhash name hash-table)
             name)))))))

(defun make-documentation-forms (namespace documentation)
  (let ((name (namespace-name namespace))
        (documentation-type (namespace-documentation-type namespace)))
    `(,@(when documentation-type
          `((defmethod documentation (name (type (eql ',documentation-type)))
              (let ((namespace (symbol-namespace ',name)))
                (values (gethash name
                                 (namespace-documentation-table namespace)))))
            (defmethod (setf documentation)
                (newdoc name (type (eql ',documentation-type)))
              (let* ((namespace (symbol-namespace ',name))
                     (doc-table (namespace-documentation-table namespace)))
                (if (null newdoc)
                    (remhash name doc-table)
                    (setf (gethash name doc-table) newdoc))))))
      ,@(when documentation
          `((setf (documentation ',name 'namespace) ,documentation))))))

(defun make-binding-table-var-forms (namespace)
  (let ((table-symbol (namespace-binding-table-var namespace))
        (hash-table-test (namespace-hash-table-test namespace)))
    `(,@(when table-symbol
          `((declaim (type hash-table ,table-symbol))
            (defvar ,table-symbol
              (make-hash-table :test ',hash-table-test)))))))

(defun make-documentation-table-var-forms (namespace)
  (let ((name (namespace-name namespace))
        (doc-table-symbol (namespace-documentation-table-var namespace)))
    `(,@(when doc-table-symbol
          `((declaim (type hash-table ,doc-table-symbol))
            (defvar ,doc-table-symbol
              (namespace-documentation-table (symbol-namespace ',name))))))))

;;; Reader forms

(defun read-evaluated-form ()
  (format *query-io* "~&;; Type a form to be evaluated:~%")
  (list (eval (read *query-io*))))

(defun make-reader-forms (namespace)
  (let ((name (namespace-name namespace))
        (table-symbol (namespace-binding-table-var namespace))
        (accessor (namespace-accessor namespace))
        (condition (namespace-condition-name namespace))
        (default-errorp (namespace-error-when-not-found-p namespace))
        (errorp-arg-p (namespace-errorp-arg-in-accessor-p namespace))
        (default-arg-p (namespace-default-arg-in-accessor-p namespace)))
    (when accessor
      `((defun ,accessor
            (name &optional
                    ,@(when errorp-arg-p `((errorp ,default-errorp errorpp)))
                    ,@(when default-arg-p `((default nil defaultp))))
          ,@(when errorp-arg-p `((declare (ignorable errorp errorpp))))
          ,@(when default-arg-p `((declare (ignorable default defaultp))))
          ,@(when table-symbol `((declare (special ,table-symbol))))
          ,(format nil
                   "Automatically defined reader function.~%~
                    ~:[Returns NIL~;Signals ~:*~S~] if the value is not found ~
                    in the namespace~:[~;, unless ERRORP is set to false~].~
                    ~:[~;~%When DEFAULT is supplied and the symbol is not ~
                    bound, the default value is automatically set.~]"
                   condition errorp-arg-p default-arg-p)
          ;; We need special treatment for namespace NAMESPACE in order to break
          ;; the metacycle in #'SYMBOL-NAMESPACE.
          (let* ((hash-table ,(or table-symbol
                                  `(namespace-binding-table
                                    ,(if (eq name 'namespace)
                                         '*namespaces*
                                         `(symbol-namespace ',name))))))
            (multiple-value-bind (value foundp) (gethash name hash-table)
              (cond (foundp value)
                    ,@(when default-arg-p
                        `((defaultp (setf (gethash name hash-table) default))))
                    ,@(when (and condition (or default-errorp errorp-arg-p))
                        `((,(cond (errorp-arg-p 'errorp)
                                  (default-errorp 't))
                           (restart-case (error ',condition :name name)
                             (use-value (newval)
                               :report "Use specified value."
                               :interactive read-evaluated-form
                               newval)
                             (store-value (newval)
                               :report "Set specified value and use it."
                               :interactive read-evaluated-form
                               (setf (gethash name hash-table)
                                     newval))))))))))))))

;;; Writer forms

(defun make-writer-forms (namespace)
  (let ((name (namespace-name namespace))
        (table-symbol (namespace-binding-table-var namespace))
        (accessor (namespace-accessor namespace))
        (errorp-arg-p (namespace-errorp-arg-in-accessor-p namespace))
        (default-arg-p (namespace-default-arg-in-accessor-p namespace)))
    (when (and accessor (not (eq name 'namespace)))
      `((defun (setf ,accessor)
            (new-value name &optional
                              ,@(when errorp-arg-p `((errorp nil)))
                              ,@(when default-arg-p `((default nil))))
          "Automatically defined writer function."
          ,@(when errorp-arg-p `((declare (ignore errorp))))
          ,@(when default-arg-p `((declare (ignore default))))
          ,@(when table-symbol `((declare (special ,table-symbol))))
          (let* ((hash-table ,(or table-symbol
                                  `(namespace-binding-table
                                    (symbol-namespace ',name)))))
            (setf (gethash name hash-table) new-value)))))))

;;; Definer forms

(defun escape-arglist (arglist)
  "Removes &whole, &aux and &environment parameters.
Removes initforms and supplied-p parameters for &optional and &key arguments.
Replaces all variables with uninterned symbols with the same name.
Returns the escaped arglist and the list of variables it contains.
Behavior is undefined if the arglist is malformed.
Returns a dummy lambda-list of (&rest #:args) if arglist is :unknown or if an
unknown (implementation-specific) lambda-list keyword is encountered."
  (let (vars)
    (labels ((fallback (&aux (args (make-symbol "ARGS")))
               (return-from escape-arglist
                 (values `(&rest ,args) `(,args))))
             (rec (arglist)
               (let* ((args (list '&required))
                      (tail args)
                      (state '&required))
                 (flet ((collect (x &rest more)
                          (setf (cdr tail) (list* x more) tail (last tail))))
                   (do () ((null arglist) (cdr args))
                     (when (symbolp arglist) ; (... . var), or  var  from a recursive call
                       (let ((var (make-symbol (symbol-name arglist))))
                         (setf (cdr tail) var)
                         (push var vars)
                         (return (cdr args))))
                     (let ((next (pop arglist)))
                       (cond
                         ((eql next '&aux) (return))
                         ((member next '(&environment &whole)) (pop arglist))
                         ((member next '(&optional &key))
                          (setf state next)
                          (collect next))
                         ((eql next '&allow-other-keys)
                          (collect next))
                         ((member next '(&rest &body))
                          (collect next (rec (pop arglist))))
                         ((member next lambda-list-keywords)
                          (warn "Unknown lambda-list keyword: ~S" next)
                          (fallback))
                         (t (ecase state
                              ;; var  or  (...)  (destructuring)
                              (&required (collect (rec next)))
                              ;; var  or  (var ...)  or  ((...) ...)
                              (&optional
                               (let ((result (rec (ensure-car next))))
                                 (collect (if (listp result) (list result) result))))
                              ;; var  or  (var ...)  or  ((key var) ...)  or  ((key (...)) ...)
                              (&key
                               (let ((key-var (ensure-car next)))
                                 (if (listp key-var)
                                     (collect `((,(first key-var) ,(rec (second key-var)))))
                                     (collect (rec key-var))))))))))))))
      (when (eql arglist :unknown)
        (fallback))
      (values (rec arglist) vars))))

(defun alias-definer-arglist (definer)
  (escape-arglist
   (etypecase definer
     (symbol
      (if (fboundp definer)
          (trivial-arguments:arglist definer)
          :unknown))
     ((cons (eql lambda))
      (second definer)))))

(defun alias-definer-form (definer definer-name accessor
                           &aux (name (make-symbol "NAME"))
                                (form (gensym "FORM")))
  (multiple-value-bind (arglist vars) (alias-definer-arglist definer)
    `(defmacro ,definer-name (&whole ,form ,name . ,arglist)
       (declare (ignore ,@vars))
       `(setf (,',accessor ',,name)
              (,',definer . ,(cddr ,form))))))

(defun simple-definer-form (definer-name accessor)
  ;; TODO: optional doc argument (like in defparameter)
  `(defmacro ,definer-name (name value)
     `(setf (,',accessor ',name) ,value)))

(defun macro-definer-form (definer definer-name accessor
                           &aux (name (make-symbol "NAME")))
  (multiple-value-bind (body declarations doc)
      ;; TODO: when alexandria updates, it would likely be preferable
      ;; to use :if-duplicate-doc-string :ignore
      (parse-body (cdr definer) :documentation t)
    (declare (ignore doc))
    `(defmacro ,definer-name (,name . ,(car definer))
       ,@declarations
       `(setf (,',accessor ',,name) ,(progn ,@body)))))

(defun make-definer-forms (namespace)
  (let ((definer-name (namespace-definer-name namespace))
        (definer (namespace-definer namespace))
        (accessor (namespace-accessor namespace)))
    (when definer-name
      `(,(etypecase definer
           ;; T
           ((eql t)
            (simple-definer-form definer-name accessor))
           ;; FOO (lambda args . body)
           ((or symbol (cons (eql lambda)))
            (alias-definer-form definer definer-name accessor))
           ;; 'FOO #'FOO '(lambda args . body) #'(lambda args . body)
           ((cons (member quote function) (cons (or symbol (cons (eql lambda))) null))
            (alias-definer-form (second definer) definer-name accessor))
           ;; (args . body)
           (cons
            (macro-definer-form definer definer-name accessor)))))))
