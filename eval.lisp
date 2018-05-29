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
  (:use :cl :scheme :alexandria :metabang-bind :bulk/reference :bulk/words :babel)
  (:shadow #:eval)
  (:export #:ns-definition #:name #:bare-id #:env
		   #:lexical-environment #:copy/do
		   #:copy/assign #:copy/assign! #:get-value #:apply-env!
		   #:copy/add-namespace #:copy/add-namespace!
		   #:find-ns #:search-ns
		   #:compound-lexical-environment #:policy/ns #:get-lasting
		   #:lex-ns #:get-lex-ns #:lex-mnemonic #:get-lex-mnemonic
		   #:lex-value #:get-lex-value #:lex-semantic #:get-lex-semantic
		   #:lex-encoding #:get-lex-encoding
		   #:qualified-ref #:dangling-ref #:qref #:dref
		   #:eager-function #:lazy-function #:impure-eager-function #:impure-lazy-function
		   #:bulk-array #:to-string
		   #:map-form #:qualify #:eval #:no-env #:eval-whole #:with-eval))

(in-package :bulk/eval)


(defclass ns-definition ()
  ((name :initarg :name)
   (bare-id :initarg :bare)
   (env :initarg :env)))

(defclass lexical-environment ()
  ((table :initform (make-hash-table :test 'equal) :initarg :table)
   (bare-ids :initform (make-hash-table :test 'equal) :initarg :bare-ids)
   (ns-search-functions :initform nil :initarg :search)))

(defgeneric set-value (env field value)
  (:documentation "Set FIELD in ENV with new value VALUE"))

(defmethod set-value ((env lexical-environment) field value)
  (setf (gethash field (slot-value env 'table)) value))


(defgeneric copy-env (env))

(defmethod copy-env ((env lexical-environment))
  (make-instance 'lexical-environment :table (copy-hash-table (slot-value env 'table))))

(defmacro copy/do ((var &optional (env nil env?)) &body body)
  `(let ((,var (copy-env ,(if env? env var))))
	 ,@body
	 ,var))

(defun copy/assign (env field value)
  (copy/do (env)
	(set-value env field value)))

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


(defgeneric add-namespace (env num definition))

(defmethod add-namespace ((env lexical-environment) num (definition ns-definition))
  (with-slots (name bare-id (defs env)) definition
	(push definition (gethash bare-id (slot-value env 'bare-ids)))
	(when num
	  (apply-env! env defs)
	  (set-value env (lex-ns num) name))))

(defun copy/add-namespace (env num definition)
  (copy/do (env)
	(add-namespace env num definition)))

(defmacro copy/add-namespace! (place num definition)
  `(setf ,place (copy/add-namespace ,place ,num ,definition)))


(defun has-ns-name? (ns-name)
  (lambda (def) (equal ns-name (slot-value def 'name))))

(defun produces-good-id? (ref-name bare-id)
  (lambda (def)
	(with-slots (name (defs env)) def
	  (copy/add-namespace! defs 40 def)
	  (equal name (eval (list (ref 40 ref-name) bare-id) defs)))))

(defun find-among-nss (nss bare-id ns-name ref-name all?)
  (cond
	(all?
	 nss)
	(ns-name
	 (find-if (has-ns-name? ns-name) nss))
	(ref-name
	 (find-if (produces-good-id? ref-name bare-id) nss))
	(t nil)))

(defgeneric find-ns (env bare-id &key ns-name ref-name)
  (:documentation "Find a namespace among the already known namespaces, by the content of its unique identifier."))

(defmethod find-ns ((env lexical-environment) bare-id &key ns-name ref-name all?)
  (if-let (found (gethash bare-id (slot-value env 'bare-ids)))
	(find-among-nss found bare-id ns-name ref-name all?)))

(defgeneric search-ns (env bare-id &key ns-name)
  (:documentation "Search for a namespace by the content of its unique identifier"))

(defmethod search-ns ((env lexical-environment) bare-id &key ns-name ref-name all?)
  (let@ rec ((functions (slot-value env 'ns-search-functions))
			 (results))
	(if-let (search-fn (first functions))
	  (if-let (found (find-among-nss (funcall search-fn bare-id) bare-id ns-name ref-name all?))
		(if all?
			(rec (rest functions) (append found results))
			found)
		(rec (rest functions) nil))
	  results)))


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


(defclass bulk-array (word)
  ((env :initarg :env)))

(defun to-string (array)
  (with-slots (bytes env) array
	(babel:octets-to-string bytes :encoding (get-value env (lex-encoding)))))


(defun map-form (function list env)
  (mapcar (lambda (obj) (funcall function obj env)) list))

(defun qualify (expr env)
  (typecase expr
	(qualified-ref expr)
	(array (make-instance 'bulk-array :bytes expr :env env))
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
	(array (qualify expr env))
	(word (unsigned-integer expr))
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
						   (values ; discard possible second value from pure lazy functions
							(if (typep function 'lazy-function)
								(apply callable env args) ; lazy function need to eval/qualify
								(apply callable args)))))
					 (map-form #'eval expr env)))))
			(t (map-form #'eval expr env))))
	(t expr)))

(defun no-env (function)
  (lambda (env &rest rest)
	(declare (ignore env))
	(apply function rest)))

(defun eval-whole (sequence env)
  (let@ rec ((sequence sequence)
			 (env env)
			 (yield))
	(if sequence
		(bind (((:values expr new-env) (eval (first sequence) env)))
		  (rec (rest sequence) (if new-env new-env env) (if expr (cons expr yield) yield)))
		(values (reverse yield) env))))

(defmacro with-eval (env bindings &body body)
  `(let (,@(mapcar (lambda (name) `(,name (eval ,name ,env)))
				   (rest (assoc 'eval bindings)))
		 ,@(mapcar (lambda (name) `(,name (to-string ,name)))
				   (rest (assoc 'string bindings))))
	 ,@body))
