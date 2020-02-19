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

(uiop:define-package :bulk/core
  (:use :cl :alexandria :bulk/eval :bulk/stringenc :bulk/reference :bulk/words :bulk/eval/helpers :bulk/spec :ieee-floats :optima)
  (:shadowing-import-from :bulk/eval #:eval)
  (:export #:*core-1.0* #:unsupported-float))

(in-package :bulk/core)


(defun copy/add-by-full-id (env num count full-id)
  (match full-id
	((list (type symbol) bare-id)
	 (if-let (def (or (find-definition env bare-id :full-id full-id)
					  (search-definition env bare-id :full-id full-id)))
	   (values nil (copy/add-definition env def :num num :count count))))))

(defun copy/add-self-describing (env num bare-id id-form &key count)
  (if-let (def (or (find-definition env bare-id :ns-num num :id-form id-form :count count)
				   (search-definition env bare-id :ns-num num :id-form id-form :count count)))
	(progn
	  (values nil (copy/add-definition env def :num num :count count)))))

(defun parse-ns (env num id-form)
  (with-eval env ((eval num))
	(match id-form
	  ((list (qualified-ref) _)
	   (copy/add-by-full-id env num nil (eval id-form env)))
	  ((list (dangling-ref (ns num2) (name _)) bare-id)
	   (if (eql num num2)
		   (copy/add-self-describing env num bare-id id-form)))
	  ((list (dangling-ref))))))

(defun parse-import (env base count id-form)
  (with-eval env ((eval base count))
 	(match id-form
	  ((list (qualified-ref) _)
	   (copy/add-by-full-id env base count (eval id-form env)))
	  ((list (dangling-ref (ns num) (name _)) bare-id)
	   (when (<= base num (+ base count))
		 (copy/add-self-describing env base bare-id id-form :count count)))
	  ((list (dangling-ref))))))

(defun define (env ref value)
  (with-eval env ((eval value))
	(with-slots (ns name) ref
	  (values value (copy/assign env (lex-value ns name) value)))))

(defun mnemonic/def (env ref mnemonic doc &optional value)
  (declare (ignore doc))
  (with-eval env ((string mnemonic)
				  (eval value))
	(with-slots (ns name) ref
	  (let ((ns-name (get-value env (lex-ns ns))))
		(copy/assign! env (lex-mnemonic ns-name name) mnemonic)
		(if value (copy/assign! env (lex-value ns-name name) value))))))


(define-condition unsupported-float (error)
  ((size :initarg :size)))

(defun binary-float (bytes)
  (let* ((%bytes (get-bytes bytes))
		 (length (length %bytes))
		 (bits (bytes->word %bytes)))
	(case length
	  (4 (decode-float32 bits))
	  (8 (decode-float64 bits))
	  (t (error 'unsupported-float :size (* 8 length))))))


(defparameter *core-1.0*
  (let ((env (make-instance 'lexical-environment)))
	(copy/assign! env '(:encoding) :utf-8)
	(copy/add-definition! env
						  (make-ns '(:std :core)
							(name nil :mnemonic "bulk")
							(name +bulk/version+ :mnemonic "version")
							(name +bulk/true+ :mnemonic "true")
							(name +bulk/false+ :mnemonic "false")
							(name +bulk/stringenc+ :mnemonic "stringenc" :semantic (fun->eager* #'stringenc))
							(name +bulk/iana-charset+ :mnemonic "iana-charset" :semantic (fun->eager #'iana-charset-mib->babel-name))
							(name +bulk/codepage+ :mnemonic "codepage" :semantic (fun->eager #'windows-code-page->babel-name))
							(name +bulk/ns+ :mnemonic "ns" :semantic (fun->lazy* #'parse-ns))
							(name +bulk/package+ :mnemonic "package")
							(name +bulk/import+ :mnemonic "import" :semantic (fun->lazy* #'parse-import))
							(name +bulk/define+ :mnemonic "define" :semantic (fun->lazy* #'define))
							(name +bulk/mnemonic/def+ :mnemonic "mnemonic/def"  :semantic (fun->lazy* #'mnemonic/def))
							(name +bulk/ns-mnemonic+ :mnemonic "ns-mnemonic")
							(name +bulk/verifiable-ns+ :mnemonic "verifiable-ns")
							(name +bulk/concat+ :mnemonic "concat" :semantic ({eager} (vector1 vector2)
																			   (make-instance 'bulk-array :env (slot-value vector1 'env)
																							  :bytes (concatenate 'vector (get-bytes vector1) (get-bytes vector2)))))
							(name +bulk/subst+ :mnemonic "subst")
							(name +bulk/arg+ :mnemonic "arg")
							(name +bulk/rest+ :mnemonic "rest")
							(name +bulk/frac+ :mnemonic "frac" :semantic ({eager}  (x y) (/ x y)))
							(name +bulk/bigint+ :mnemonic "bigint" :semantic (fun->lazy #'signed-integer))
							(name +bulk/binary+ :mnemonic "binary" :semantic (fun->lazy #'binary-float))
							(name +bulk/decimal+ :mnemonic "decimal")
							(name +bulk/prefix-bytecode+ :mnemonic "prefix-bytecode")
							(name +bulk/prefix-bytecode*+ :mnemonic "prefix-bytecode*")
							(name +bulk/postfix-bytecode+ :mnemonic "postfix-bytecode")
							(name +bulk/postfix-bytecode*+ :mnemonic "postfix-bytecode*")
							(name +bulk/arity+ :mnemonic "arity")
							(name +bulk/property-list+ :mnemonic "property-list"))
						  :num +bulk/+)
	env))
