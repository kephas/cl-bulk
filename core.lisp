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
  (:use :cl :bulk/eval :bulk/stringenc :bulk/reference)
  (:shadowing-import-from :bulk/eval #:eval)
  (:export #:*core-1.0*))

(in-package :bulk/core)

(defvar *core-1.0* (make-instance 'lexical-environment))
(eval-when (:compile-toplevel)
  (defconstant +core+ '(:std :core)))

(copy/assign! *core-1.0* '(:marker #x20) +core+)
(copy/assign! *core-1.0* `(:mnemonic ,+core+) "bulk")

(dolist (pair '((#x0 "version")
				(#x1 "true")
				(#x2 "false")
				(#x3 "stringenc")
				(#x4 "iana-charset")
				(#x5 "codepage")
				(#x6 "ns")
				(#x7 "package")
				(#x8 "import")
				(#x9 "define")
				(#xA "mnemonic/def")
				(#xB "ns-mnemonic")
				(#xC "verifiable-ns")
				(#x10 "concat")
				(#x11 "subst")
				(#x12 "arg")
				(#x13 "rest")
				(#x20 "frac")
				(#x21 "bigin")
				(#x22 "binary")
				(#x23 "decimal")
				(#x30 "prefix-bytecode")
				(#x31 "prefix-bytecode*")
				(#x32 "postfix-bytecode")
				(#x33 "postfix-bytecode*")
				(#x34 "arity")
				(#x35 "property-list")))
  (copy/assign! *core-1.0* `(:mnemonic ,+core+ ,(first pair)) (second pair)))

(copy/assign! *core-1.0* '(:encoding) :utf-8)

(copy/assign! *core-1.0* (lex-semantic +core+ #x3) *stringenc*)
(copy/assign! *core-1.0* (lex-semantic +core+ #x4) *iana*)
(copy/assign! *core-1.0* (lex-semantic +core+ #x5) *codepage*)


(defun define (env ref value)
  (let ((ref (qualify ref env))
		(value (eval value env)))
	(with-slots (ns name) ref
	  (values value (copy/assign env (lex-value ns name) value)))))

(copy/assign! *core-1.0* (lex-semantic +core+ #x9) (make-instance 'impure-lazy-function :fun #'define))
