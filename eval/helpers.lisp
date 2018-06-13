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

(uiop:define-package :bulk/eval/helpers
  (:use :cl :scheme :alexandria :bulk/eval :bulk/reference)
  (:shadowing-import-from #:bulk/eval #:eval)
  (:export #:no-env #:fun->eager #:fun->lazy
		   #:{eager} #:{eager*} #:{lazy} #:{lazy*}
		   #:set-ns! #:make-ns #:name))

(in-package :bulk/eval/helpers)


(defun no-env (function)
  (lambda (env &rest rest)
	(declare (ignore env))
	(apply function rest)))

(defun fun->eager (function)
  (make-instance 'eager-function :fun function))

(defun fun->lazy (function)
  (make-instance 'lazy-function :fun (no-env function)))


(defmacro make-function-maker (name class)
  (list 'defmacro name '(args &body body)
		(list 'list ''make-instance (list 'quote class) :fun '(cons 'lambda (cons args body)))))

(make-function-maker {eager} 'eager-function)
(make-function-maker {lazy} 'lazy-function)
(make-function-maker {eager*} 'impure-eager-function)
(make-function-maker {lazy*} 'impure-lazy-function)


(defmacro set-ns! (ns &body body)
  (with-gensyms (full-id env num fields)
	`(with-slots ((,full-id full-id) (,env env)) ,ns
	   (labels ((name (,num &rest ,fields)
				  (let@ rec ((,fields ,fields))
					(when ,fields
					  (copy/assign! ,env (list (first ,fields) ,full-id ,num) (second ,fields))
					  (rec (cddr ,fields))))))
		 ,@body))))

(defmacro make-ns (full-id &body body)
  (with-gensyms (ns)
	`(let ((,ns (make-instance 'ns-definition  :full ,full-id :bare (second ,full-id) :env (make-instance 'lexical-environment))))
	   (set-ns! ,ns ,@body)
	   ,ns)))
