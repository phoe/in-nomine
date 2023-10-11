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

(defun normalize-arglist (arglist)
  "Makes sure the argument list contains an &REST parameter for &KEY and
&OPTIONAL parameters"
  (multiple-value-bind (required optional rest keywords allow-other-keys? aux
                        keys?)
      (parse-ordinary-lambda-list arglist
                                  :normalize nil)
    (let ((rest-arg (or rest (gensym "rest"))))
      (values `(,@required
                ,@(when optional
                    `(&optional))
                ,@optional
                &rest ,rest-arg
                ,@(when keys?
                    `(&key))
                ,@keywords
                ,@(when allow-other-keys?
                    `(&allow-other-keys))
                ,@(when aux
                    `(&aux))
                ,@aux)
              rest-arg
              `(,@required ,@(mapcar #'ensure-car
                                     optional))
              (mapcar #'ensure-car
                      keywords)))))

(defun construct-function-definer-form (function name accessor)
  (let ((g!name (gensym "name")))
    (multiple-value-bind (arglist rest-argument normal-args k/o-args)
        (normalize-arglist (cond
                             ((symbolp function)
                              (t:arglist function))
                             ((and (listp function)
                                   (eq (first function)
                                       'lambda))
                              (second function))
                             (t (error "Malformed function name ~S while ~
                                        building the definer for ~S"
                                       function name))))
      `(defmacro ,name (,g!name ,@arglist)
         (declare ,@(mapcar (lambda (k/o-arg)
                              `(ignore ,k/o-arg))
                            k/o-args))
         `(setf (,',accessor ',,g!name)
                (,',(if (and (listp function)
                             (eq (first function)
                                 function))
                        (second function)
                        function)
                 ,,@normal-args . ,,rest-argument))))))

(defun make-definer-forms (namespace)
  (let ((g!name (gensym "name"))
        (name (namespace-definer-name namespace))
        (definer (namespace-definer namespace))
        (accessor (namespace-accessor namespace)))
    (when name
      (typecase definer
        ((eql t)
         `((defmacro ,name (,g!name obj)
             `(setf (,',accessor ',,g!name)
                    ,obj))))
        (symbol
         `(,(construct-function-definer-form definer name accessor)))
        (list
         (etypecase (first definer)
           ((or (eql function)
                (eql quote))
            `(,(construct-function-definer-form (second definer)
                                                name accessor)))
           ((eql lambda)
            `(,(construct-function-definer-form definer name accessor)))
           (list
            `((defmacro ,name (,g!name ,@(first definer))
                (let ((g!object (gensym "object")))
                  `(let ((,g!object (progn
                                      ,,@(rest definer))))
                     (setf (,',accessor ',,g!name)
                           ,g!object)
                     ,g!object)))))))))))

;;; Let form

(defmacro mlet ((name lambda-list &body body) &body mlet-body
                &environment env)
  `(macrolet ((,name ,lambda-list
                `(macrolet ((,',name (&whole form &rest args &environment env)
                              (declare (ignore args))
                              (funcall ,',(macro-function name env) form env)))
                   ,(progn ,@body))))
     ,@mlet-body))

(defun get-declared (declarations decl-type test)
  (remove-duplicates
   (loop for (nil . decls) in declarations
         append (loop for (type . args) in decls
                      when (eq type decl-type)
                      append args))
   :test test))

(defun make-let-forms (namespace)
  (let ((let-name (namespace-let-name namespace))
        (accessor (namespace-macro-accessor namespace))
        (global-accessor (namespace-accessor namespace))
        (boundp (namespace-boundp-symbol namespace))
        (makunbound (namespace-makunbound-symbol namespace))
        (namespace-name (namespace-name namespace))
        (test (namespace-hash-table-test namespace))
        (condition (namespace-condition-name namespace))
        (default-errorp (namespace-error-when-not-found-p namespace))
        (errorp-arg-p (namespace-errorp-arg-in-accessor-p namespace))
        (default-arg-p (namespace-default-arg-in-accessor-p namespace)))
    (when (and let-name accessor global-accessor boundp makunbound)
      `((defmacro ,accessor (&whole form name
                             &optional
                               ,@(when errorp-arg-p `((errorp ,default-errorp)))
                               ,@(when default-arg-p `((default nil))))
          (declare (ignore ,@(when errorp-arg-p `(errorp))
                           ,@(when default-arg-p `(default))))
          ,(format nil
                   "Automatically defined accessor macro.~%~
                    ~:[Returns NIL~;Signals ~:*~S~] if the value is not found ~
                    in the namespace~:[~;, unless ERRORP is set to false~].~
                    ~:[~;~%When DEFAULT is supplied and the symbol is not ~
                    bound, the default value is automatically set.~]"
                   condition errorp-arg-p default-arg-p)
          `(,',global-accessor ',name ,@(cddr form)))
        (defmacro ,let-name (bindings &body body)
          (multiple-value-bind (body decls) (parse-body body)
            (let* ((specials (get-declared decls 'special ',test))
                   (ignorables (get-declared decls 'ignorable ',test))
                   (ignored (get-declared decls 'ignore ',test))
                   (variables (loop for (name value) in (mapcar #'ensure-list bindings)
                                    collect (list (gensym (with-standard-io-syntax
                                                            (format nil "~A-~A"
                                                                    ',namespace-name
                                                                    name)))
                                                  value
                                                  name
                                                  (member name specials :test ',test)
                                                  (member name ignorables :test ',test)
                                                  (member name ignored :test ',test))))
                   (switch-clauses (loop for (var value name special-p ignorable-p ignore-p) in variables
                                         unless special-p
                                           collect `(',name ',var)))
                   (unbound-marker (gensym (with-standard-io-syntax
                                             (format nil "~A-UNBOUND" ',namespace-name)))))
              `(let (,@(loop for (var value name special-p ignorable-p ignore-p) in variables
                             collect (if special-p
                                         `(,var (if (,',boundp ',name)
                                                    (,',global-accessor ',name)
                                                    ',unbound-marker))
                                         `(,var))))
                 (declare
                  (ignorable ,@(loop for (var value name special-p ignorable-p ignore-p) in variables
                                     when (or ignore-p ignorable-p)
                                       collect var)))
                 (unwind-protect
                      (progn
                        (psetf ,@(loop for (var value name special-p ignorable-p ignore-p) in variables
                                       collect (if special-p
                                                   `(,',global-accessor ',name)
                                                   var)
                                       collect value))
                        (mlet (,',accessor (name &rest args)
                                ,(if switch-clauses  ; workaround alexandria's extra style-warning bug
                                     `(switch (name :test ,',test)
                                        ,@switch-clauses
                                        (t `(,',',accessor ,name ,@args)))
                                     ``(,',',accessor ,name ,@args)))
                          ,@body))
                   ,@(loop for (var value name special-p ignorable-p ignore-p) in variables
                           when special-p
                             collect `(if (eq ,var ',unbound-marker)
                                          (,',makunbound ',name)
                                          (setf (,',global-accessor ',name) ,var))))))))))))
