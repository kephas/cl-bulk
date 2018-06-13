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
  (:use :cl :alexandria :bulk/eval :bulk/stringenc :bulk/reference :bulk/words :bulk/eval/helpers :ieee-floats :optima)
  (:shadowing-import-from :bulk/eval #:eval)
  (:export #:*core-1.0* #:unsupported-float))

(in-package :bulk/core)

(defvar *core-1.0* (make-instance 'lexical-environment))
(copy/assign! *core-1.0* '(:encoding) :utf-8)


(defun copy/add-by-full-id (env num count full-id)
  (match full-id
	((list (type symbol) bare-id)
	 (if-let (def (or (find-definition env bare-id :full-id full-id)
					  (search-definition env bare-id :full-id full-id)))
	   (values nil (copy/add-definition env def :num num :count count))))))

(defun copy/add-self-describing (env num ref-name bare-id)
  (if-let (def (or (find-definition env bare-id :ref-name ref-name)
				   (search-definition env bare-id :ref-name ref-name)))
	(values nil (copy/add-definition env def :num num))))

(defun parse-ns (env num id-form)
  (with-eval env ((eval num))
	(match id-form
	  ((list (qualified-ref) _)
	   (copy/add-by-full-id env num nil (eval id-form env)))
	  ((list (dangling-ref (ns num) (name ref-name)) bare-id) (copy/add-self-describing env num ref-name bare-id))
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


(copy/add-definition! *core-1.0*
					  (make-ns '(:std :core)
						(name nil :mnemonic "bulk")
						(name #x0 :mnemonic "version")
						(name #x1 :mnemonic "true")
						(name #x2 :mnemonic "false")
						(name #x3 :mnemonic "stringenc" :semantic (fun->eager* #'stringenc))
						(name #x4 :mnemonic "iana-charset" :semantic (fun->eager #'iana-charset-mib->babel-name))
						(name #x5 :mnemonic "codepage" :semantic (fun->eager #'windows-code-page->babel-name))
						(name #x6 :mnemonic "ns" :semantic (fun->lazy* #'parse-ns))
						(name #x7 :mnemonic "package")
						(name #x8 :mnemonic "import")
						(name #x9 :mnemonic "define" :semantic (fun->lazy* #'define))
						(name #xA :mnemonic "mnemonic/def"  :semantic (fun->lazy* #'mnemonic/def))
						(name #xB :mnemonic "ns-mnemonic")
						(name #xC :mnemonic "verifiable-ns")
						(name #x10 :mnemonic "concat" :semantic ({eager} (vector1 vector2)
																  (make-instance 'bulk-array :env (slot-value vector1 'env)
																				 :bytes (concatenate 'vector (get-bytes vector1) (get-bytes vector2)))))
						(name #x11 :mnemonic "subst")
						(name #x12 :mnemonic "arg")
						(name #x13 :mnemonic "rest")
						(name #x20 :mnemonic "frac" :semantic ({eager}  (x y) (/ x y)))
						(name #x21 :mnemonic "bigint" :semantic (fun->lazy #'signed-integer))
						(name #x22 :mnemonic "binary" :semantic (fun->lazy #'binary-float))
						(name #x23 :mnemonic "decimal")
						(name #x30 :mnemonic "prefix-bytecode")
						(name #x31 :mnemonic "prefix-bytecode*")
						(name #x32 :mnemonic "postfix-bytecode")
						(name #x33 :mnemonic "postfix-bytecode*")
						(name #x34 :mnemonic "arity")
						(name #x35 :mnemonic "property-list"))
					  :num #x20)
