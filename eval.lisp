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
  (:export #:definition #:ns-definition #:pkg-definition #:full-id #:bare-id #:env #:children
		   #:lexical-environment #:copy/do
		   #:copy/assign #:copy/assign! #:get-value #:apply-env!
		   #:copy/add-definition #:copy/add-definition!
		   #:find-definition #:search-definition
		   #:compound-lexical-environment #:policy/ns #:get-lasting
		   #:lex-ns #:get-lex-ns #:lex-mnemonic #:get-lex-mnemonic
		   #:lex-value #:get-lex-value #:lex-semantic #:get-lex-semantic
		   #:lex-encoding #:get-lex-encoding
		   #:qualified-ref #:dangling-ref #:qref #:dref
		   #:eager-function #:lazy-function #:impure-eager-function #:impure-lazy-function
		   #:bulk-array #:to-string
		   #:map-form #:qualify #:eval #:no-env #:eval-whole #:with-eval))

(in-package :bulk/eval)


(defclass definition ()
  ((full-id :initarg :full)
   (bare-id :initarg :bare)))

(defclass ns-definition (definition)
   ((env :initarg :env)))

(defclass pkg-definition (definition)
  ((namespaces :initarg :nss)))

(defclass lexical-environment ()
  ((table :initform (make-hash-table :test 'equal) :initarg :table)
   (bare-ids :initform (make-hash-table :test 'equal) :initarg :bare-ids)
   (def-search-functions :initform nil :initarg :search)))

(defgeneric set-value (env field value)
  (:documentation "Set FIELD in ENV with new value VALUE"))

(defmethod set-value ((env lexical-environment) field value)
  (setf (gethash field (slot-value env 'table)) value))


(defgeneric copy-env (env))

(defmethod copy-env ((env lexical-environment))
  (make-instance 'lexical-environment
				 :table (copy-hash-table (slot-value env 'table))
				 :bare-ids (copy-hash-table (slot-value env 'bare-ids))
				 :search (slot-value env 'def-search-functions)))

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


(defgeneric add-definition (env definition &key num count))

(defmethod add-definition ((env lexical-environment) (definition ns-definition) &key num count)
  (declare (ignore count))
  (with-slots (full-id bare-id (defs env)) definition
	(push definition (gethash bare-id (slot-value env 'bare-ids)))
	(when num
	  (apply-env! env defs)
	  (set-value env (lex-ns num) full-id))))

(defmethod add-definition ((env lexical-environment) (definition pkg-definition) &key num count)
  (with-slots (full-id bare-id namespaces) definition
	(push definition (gethash bare-id (slot-value env 'bare-ids)))
	(when num
	  (let@ rec ((num num)
				 (count count)
				 (nss namespaces))
		(unless (zerop count)
		  (add-definition env (first nss) :num num)
		  (rec (1+ num) (1- count) (rest nss)))))))

(defun copy/add-definition (env definition &rest rest)
  (copy/do (env)
	(apply #'add-definition env definition rest)))

(defmacro copy/add-definition! (place definition &rest rest)
  `(setf ,place (copy/add-definition ,place ,definition ,@rest)))


(defun has-full-id? (full-id)
  (lambda (def)
	(equal full-id (slot-value def 'full-id))))

(defun produces-good-id? (ref-name bare-id)
  (lambda (def)
	(and (typep def 'ns-definition)
		 (with-slots (full-id env) def
		   (copy/add-definition! env def :num 40)
		   (equal full-id (eval (list (ref 40 ref-name) bare-id) env))))))

(defun find-among-defs (defs bare-id full-id ref-name all?)
  (cond
	(all?
	 defs)
	(full-id
	 (find-if (has-full-id? full-id) defs))
	(ref-name
	 (find-if (produces-good-id? ref-name bare-id) defs))
	(t nil)))

(defgeneric find-definition (env bare-id &key full-id ref-name)
  (:documentation "Find a namespace among the already known namespaces, by the content of its unique identifier."))

(defmethod find-definition ((env lexical-environment) bare-id &key full-id ref-name all?)
  (if-let (found (gethash bare-id (slot-value env 'bare-ids)))
	(find-among-defs found bare-id full-id ref-name all?)))

(defgeneric search-definition (env bare-id &key full-id)
  (:documentation "Search for a namespace by the content of its unique identifier"))

(defmethod search-definition ((env lexical-environment) bare-id &key full-id ref-name all?)
  (let@ rec ((functions (slot-value env 'def-search-functions))
			 (results))
	(if-let (search-fn (first functions))
	  (if-let (found (find-among-defs (funcall search-fn bare-id) bare-id full-id ref-name all?))
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
  (with-slots (lasting-env) source
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
					 (eval-list expr env)))))
			(t (eval-list expr env))))
	(t expr)))

(defun eval-list (list env)
  (values (eval-whole list env) env))

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
