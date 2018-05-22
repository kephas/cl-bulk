 #| BULK library
    Copyright (C) 2013--2018 Pierre Thierry <pierre@nothos.net>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>. |#

(uiop:define-package :bulk/eval
  (:use :cl :scheme :alexandria :metabang-bind :bulk/reference)
  (:shadow #:eval)
  (:export #:lexical-environment #:copy/assign #:copy/assign! #:get-value #:apply-env!
		   #:compound-lexical-environment #:policy/ns #:get-lasting
		   #:lex-ns #:get-lex-ns #:lex-mnemonic #:get-lex-mnemonic
		   #:lex-value #:get-lex-value #:lex-semantic #:get-lex-semantic
		   #:lex-encoding #:get-lex-encoding
		   #:qref #:dref
		   #:eager-function #:lazy-function #:impure-eager-function #:impure-lazy-function
		   #:map-form #:qualify #:eval #:eval-whole))

(in-package :bulk/eval)


(defclass lexical-environment ()
  ((table :initform (make-hash-table :test 'equal) :initarg :table)))

(defgeneric set-value (env field value)
  (:documentation "Set FIELD in ENV with new value VALUE"))

(defmethod set-value ((env lexical-environment) field value)
  (setf (gethash field (slot-value env 'table)) value))


(defgeneric copy-env (env))

(defmethod copy-env ((env lexical-environment))
  (make-instance 'lexical-environment :table (copy-hash-table (slot-value env 'table))))

(defun copy/assign (env field value)
  (let ((new-env (copy-env env)))
	(set-value new-env field value)
	new-env))

(defmacro copy/assign! (place field value)
  `(setf ,place (copy/assign ,place ,field ,value)))

(defgeneric get-value (env field))

(defmethod get-value ((env lexical-environment) field)
  (gethash field (slot-value env 'table)))


(defgeneric apply-env! (target source)
  (:documentation "Apply all assignments in SOURCE to TARGET."))

(defmethod apply-env! (target (source lexical-environment))
  (maphash (lambda (field value)
			 (set-value target field value))
		   (slot-value source 'table)))


(defclass compound-lexical-environment ()
  ((normal-env :initarg :normal)
   (lasting-env :initform (make-instance 'lexical-environment) :initarg :lasting :reader get-lasting)
   (policy :initarg :policy)))

(defmethod get-value ((env compound-lexical-environment) field)
  (with-slots (normal-env lasting-env) env
	(bind (((:values value found?) (get-value lasting-env field)))
	  (if found?
		  value
		  (get-value normal-env field)))))

(defmethod set-value ((env compound-lexical-environment) field value)
  (with-slots (normal-env lasting-env policy) env
	(if (funcall policy field)
		(set-value lasting-env field value)
		(set-value normal-env field value))))

(defmethod copy-env ((env compound-lexical-environment))
  (with-slots (normal-env lasting-env policy) env
	(make-instance 'compound-lexical-environment
				   :normal (copy-env normal-env)
				   :lasting (copy-env lasting-env)
				   :policy policy)))

(defmethod apply-env! (target (source compound-lexical-environment))
  (with-slots (normal-env lasting-env) source
	(apply-env! target normal-env)
	(apply-env! target lasting-env)))

(defun policy/ns (ns)
  (lambda (field)
	(and (member (first field) '(:mnemonic :value :semantic))
		 (equal ns (second field)))))



(defmacro lex-field (name symbol (&rest params))
  (let ((field (intern (format nil "LEX-~a" name)))
		(get (intern (format nil "GET-LEX-~a" name))))
	`(progn
	   (defun ,field (,@params)
		 (list ,symbol ,@params))
	   (defun ,get (env ,@params)
		 (get-value env (,field ,@params))))))

(lex-field "NS" :marker (number))
(lex-field "MNEMONIC" :mnemonic (ns name))
(lex-field "VALUE" :value (ns name))
(lex-field "SEMANTIC" :semantic (ns name))
(lex-field "ENCODING" :encoding ())


(defclass qualified-ref (ref) ()
  (:documentation "A qualified reference is a reference that has a namespace associated to its marker."))

(defun qref (ns name)
  (make-instance 'qualified-ref :ns ns :name name))

(defmethod ref-constructor ((ref qualified-ref))
  'qref)

(defclass dangling-ref (ref) ()
  (:documentation "A dangling reference is a reference that has no namespace associated to its marker."))

(defun dref (ns name)
  (make-instance 'dangling-ref :ns ns :name name))

(defmethod ref-constructor ((ref dangling-ref))
  'dref)



(defclass bulk-function ()
  ((function :initarg :fun)))

(defclass eager-function (bulk-function) ())
(defclass lazy-function (bulk-function) ())

(defclass impure-function (bulk-function) ()
  (:documentation "An impure function has side-effects on the lexical environment. Its Lisp function will get the environment as its first argument."))
(defclass impure-eager-function (eager-function impure-function) ())
(defclass impure-lazy-function (lazy-function impure-function) ())



(defun map-form (function list env)
  (mapcar (lambda (obj) (funcall function obj env)) list))

(defun qualify (expr env)
  (typecase expr
	(qualified-ref expr)
	(ref (with-slots (ns name) expr
		   (if-let (ns* (get-lex-ns env ns))
			 (qref ns* name)
			 (dref ns name))))
	(list (map-form #'qualify expr env))
	(t expr)))

(defun eval (expr env)
  (typecase expr
	(qualified-ref (with-slots (ns name) expr
					 (if-let (value (get-lex-value env ns name))
					   value
					   expr)))
	(dangling-ref expr)
	(ref (eval (qualify expr env) env))
	(list (cond
			((null expr) nil)
			((typep (first expr) 'ref)
			 (with-slots (ns name) (qualify (first expr) env)
			   (let* ((function (if-let (semantic (get-lex-semantic env ns name))
								  semantic
								  (if-let (value (get-lex-value env ns name))
									(if (typep value 'bulk-function)
										value))))
					  (args-handler (typecase function
									  (eager-function #'eval)
									  (lazy-function #'qualify))))
				 (if function
					 (let ((args (map-form args-handler (rest expr) env))
						   (callable (slot-value function 'function)))
					   (if (typep function 'impure-function)
						   (apply callable env args)
						   (apply callable args)))
					 (map-form #'eval expr env)))))
			(t (map-form #'eval expr env))))
	(t expr)))

(defun eval-whole (sequence env)
  (let@ rec ((sequence sequence)
			 (env env)
			 (yield))
	(if sequence
		(bind (((:values expr new-env) (eval (first sequence) env)))
		  (rec (rest sequence) (if new-env new-env env) (if expr (cons expr yield) yield)))
		(reverse yield))))
