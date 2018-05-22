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

(defpackage :bulk/test
  (:use :cl :hu.dwim.stefil :metabang-bind :bulk/read :bulk/write :bulk/words :bulk/eval :bulk/core :scheme :flexi-streams)
  (:shadowing-import-from :bulk/eval #:eval)
  (:export #:all #:maths #:parsing #:writing #:evaluation))

(in-package :bulk/test)


(defparameter *primitives-bulk* #(1 0 3 4 12 72 101 108 108 111 32 119
111 114 108 100 33 4 42 5 1 0 6 1 0 0 0 7 1 35 69 103 137 171 205 239
9 128 2))
(defparameter *primitives* '((:nil #(#x48 #x65 #x6C #x6C #x6F #x20
#x77 #x6F #x72 #x6C #x64 #x21) #x2A #x100 #x1000000 #x123456789ABCDEF
#x-80)))

(defparameter *nesting-bulk* #(0 1 2 1 1 2 1 2 2))
(defparameter *nesting* '(:nil nil (nil nil)))

(defparameter *references-bulk* #(32 1 32 2 254 255 255 255 188 128))
(defparameter *references* (list (ref #x20 #x1)
				 (ref #x20 #x2)
				 (ref #xFE #xFF)
				 (ref #x2BA #x80)))

(defun read-bulk-seq (seq)
  (with-input-from-sequence (in seq)
    (read-whole in :version '(1 0))))


#| custom equality predicate |#

(defgeneric egal? (x y))

(defmethod egal? (x y)
  (equalp x y))

(defmethod egal? ((x (eql nil)) (y (eql nil)))
  t)

(defmethod egal? ((x list) (y list))
  (and (egal? (first x) (first y))
       (egal? (rest x) (rest y))))

(defmethod egal? ((x ref) (y ref))
  (and (eq (type-of x) (type-of y))
	   (egal? (slot-value x 'ns)
			  (slot-value y 'ns))
       (egal? (slot-value x 'name)
			  (slot-value y 'name))))


#| test suite |#

(defsuite* all)

(in-suite all)
(defsuite* maths)

(defparameter *specs-2c* '((-5 #xFB 1)(0 0 1)(0 0 4)(#x-80 #x80 1)(#x-7F #x81 1)(#x7F #x7F 1)(#x-7F #xFF81 2)))

(deftest make-2c ()
  (dolist (specs *specs-2c*)
    (is (= (make-2c-notation (first specs) (third specs)) (second specs)))))

(deftest parse-2c ()
  (dolist (specs *specs-2c*)
    (is (= (parse-2c-notation (second specs) (third specs)) (first specs)))))


(in-suite all)
(defsuite* parsing)

(deftest read-primitives ()
  (is (egal? *primitives* (read-bulk-seq *primitives-bulk*))))

(deftest read-nesting ()
  (is (egal? *nesting* (read-bulk-seq *nesting-bulk*))))

(deftest read-references ()
  (is (egal? *references* (read-bulk-seq *references-bulk*))))

(in-suite all)
(defsuite* writing)

(deftest write-primitives ()
  (is (egal? *primitives-bulk* (with-output-to-sequence (out)
				 (write-whole out *primitives*)))))

(deftest write-nesting ()
  (is (egal? *nesting-bulk* (with-output-to-sequence (out)
				 (write-whole out *nesting*)))))

(deftest write-references ()
  (is (egal? *references-bulk* (with-output-to-sequence (out)
				 (write-whole out *references*)))))

(in-suite all)
(defsuite* evaluation)

(deftest assign ()
  (let ((env (make-instance 'lexical-environment)))
	(copy/assign! env "foo" 2)
	(copy/assign! env "bar" 3)
	(is (= 2 (get-value env "foo")))
	(is (= 3 (get-value env "bar")))
	(is (not (get-value env "quux")))))

(deftest immutability ()
  (let* ((env1 (copy/assign (make-instance 'lexical-environment) "foo" 10))
		 (env2 (copy/assign env1 "foo" 20)))
	(is (= 10 (get-value env1 "foo")))
	(is (= 20 (get-value env2 "foo")))))

(deftest compound ()
  (let ((env (make-instance 'compound-lexical-environment
							:normal (copy/assign (make-instance 'lexical-environment) '(:value (:foo :bar) 34) 0)
							:policy (policy/ns '(:foo :bar)))))
	(copy/assign! env '(:value (:foo :bar) 34) 43)
	(copy/assign! env '(:value (:foo :quux) 34) 54)
	(is (= 43 (get-value (get-lasting env) '(:value (:foo :bar) 34))))
	(is (not (get-value (get-lasting env) '(:value (:foo :quux) 34))))))

(deftest env-application ()
  (let ((env1 (make-instance 'lexical-environment))
		(env2 (make-instance 'lexical-environment)))
	(copy/assign! env1 (lex-value '(:foo :bar) 0) 0)
	(is (= 0 (get-value env1 (lex-value '(:foo :bar) 0))))
	(copy/assign! env2 (lex-value '(:foo :bar) 0) 42)
	(copy/assign! env2 (lex-value '(:foo :bar) 1) 1000)
	(apply-env! env1 env2)
	(is (= 42 (get-value env1 (lex-value '(:foo :bar) 0))))
	(is (= 1000 (get-value env1 (lex-value '(:foo :bar) 1)))))
  (let ((env1 (make-instance 'compound-lexical-environment :normal *core-1.0* :policy (policy/ns '(:foo :bar))))
		(env2 (make-instance 'compound-lexical-environment :normal *core-1.0* :policy (policy/ns '(:foo :quux)))))
	(copy/assign! env1 (lex-value '(:foo :bar) 0) 0)
	(copy/assign! env1 (lex-value '(:foo :quux) 0) 1)
	(is (= 0 (get-value env1 (lex-value '(:foo :bar) 0))))
	(is (= 1 (get-value env1 (lex-value '(:foo :quux) 0))))
	(copy/assign! env2 (lex-value '(:foo :bar) 0) 42)
	(copy/assign! env2 (lex-value '(:foo :bar) 1) 1000)
	(copy/assign! env2 (lex-value '(:foo :quux) 0) 1024)
	(apply-env! env1 env2)
	(is (= 42 (get-value env1 (lex-value '(:foo :bar) 0))))
	(is (= 1000 (get-value env1 (lex-value '(:foo :bar) 1))))
	(is (= 1024 (get-value env1 (lex-value '(:foo :quux) 0))))))

(deftest eval-one ()
  (let ((env *core-1.0*))
	(copy/assign! env (lex-ns 99) '(:foo :bar))
	(copy/assign! env (lex-value '(:foo :bar) 3) "quux")
	(copy/assign! env (lex-semantic '(:foo :bar) 4) (make-instance 'eager-function :fun #'+))
	(copy/assign! env (lex-semantic '(:foo :bar) 5) (make-instance 'eager-function :fun #'append))
	(copy/assign! env (lex-semantic '(:foo :bar) 6) (make-instance 'lazy-function :fun #'append))
	(is (egal? (dref 42 0) (eval (ref 42 0) env)))
	(is (egal? (qref '(:std :core) 2) (eval (ref 32 2) env)))
	(is (egal? "quux" (eval (ref 99 3) env)))
	(is (egal? 6 (eval (list (ref 99 4) 1 2 3) env)))
	(is (egal? (eval (list (ref 99 5) (list (list (ref 99 4) 1 2)) (list 3)) env) (list 3 3)))
	(is (egal? (eval (list (ref 99 6) (list (list (ref 99 4) 1 2)) (list 3)) env) (list (list (qref '(:foo :bar) 4) 1 2) 3)))))

(deftest eval-many ()
  (let ((env *core-1.0*))
	(copy/assign! env (lex-ns 255) '(:foo :bar))
	(copy/assign! env (lex-semantic '(:foo :bar) 0) (make-instance 'eager-function :fun #'+))
	(is (egal? '(1 2 3 4 10) (eval-whole `((,(ref 32 9) ,(ref 255 10) 1)
										   (,(ref 32 9) ,(ref 255 20) 2)
										   (,(ref 32 9) ,(ref 255 30) 3)
										   (,(ref 32 9) ,(ref 255 40) 4)
										   (,(ref 255 0)
											 (,(ref 255 0) ,(ref 255 10) ,(ref 255 20))
											 (,(ref 255 0) ,(ref 255 30) ,(ref 255 40)))) env)))))

(defsuite* core)

(deftest stringenc ()
  (is (eq :iso-8859-15
		 (bind (((:values _ env) (eval (list (ref 32 3) (list (ref 32 4) 111)) *core-1.0*)))
		   (get-lex-encoding env))))
  (is (eq :utf-8
		  (bind (((:values _ env) (eval (list (ref 32 3) (list (ref 32 4) 106)) *core-1.0*)))
			(get-lex-encoding env))))
  (is (eq :utf-32
		  (bind (((:values _ env) (eval (list (ref 32 3) (list (ref 32 5) 12000)) *core-1.0*)))
			(get-lex-encoding env))))
  (is (eq :us-ascii
		  (bind (((:values _ env) (eval (list (ref 32 3) (list (ref 32 5) 20127)) *core-1.0*)))
			(get-lex-encoding env)))))

