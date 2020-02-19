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
  (:use :cl :hu.dwim.stefil :metabang-bind :bulk/read :bulk/write :bulk/words :bulk/eval :bulk/eval/helpers :bulk/core :scheme :flexi-streams)
  (:shadowing-import-from :bulk/eval #:eval)
  (:export #:all #:maths #:parsing #:writing #:evaluation))

(in-package :bulk/test)


(defparameter *primitives* '((:nil #(0))
							 (nil #(1 2))
							 (#(1 2 3) #(3 4 3 1 2 3))
							 (0 #(4 0))
							 (#x0F #(4 #x0F))
							 (#x0F1F #(5 #x0F #x1F))
							 (#x1F2F3F #(6 0 #x1F #x2F #x3F))
							 (#x0F1F2F3F #(6 #x0F #x1F #x2F #x3F))
							 (#x0F1F2F3F4F5F6F7F #(7 #x0F #x1F #x2F #x3F #x4F #x5F #x6F #x7F))
							 (#x0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFFF #(8 #x0F #x1F #x2F #x3F #x4F #x5F #x6F #x7F #x8F #x9F #xAF #xBF #xCF #xDF #xEF #xFF))
							 (#x-0F #(9 #x0F))
							 (#x-0F1F #(10 #x0F #x1F))
							 (#x-1F2F3F #(11 0 #x1F #x2F #x3F))
							 (#x-0F1F2F3F #(11 #x0F #x1F #x2F #x3F))
							 (#x-0F1F2F3F4F5F6F7F #(12 #x0F #x1F #x2F #x3F #x4F #x5F #x6F #x7F))
							 (#x-0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFFF #(13 #x0F #x1F #x2F #x3F #x4F #x5F #x6F #x7F #x8F #x9F #xAF #xBF #xCF #xDF #xEF #xFF))))

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

(defun resolve-words (yield)
  (mapcar (lambda (obj)
			(typecase obj
			  (word (unsigned-integer obj))
			  (list (resolve-words obj))
			  (t obj)))
		  yield))

(defun same? (expr1 expr2)
  (string-equal (format nil "~a" expr1) (format nil "~a" expr2)))


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

(defparameter *specs-2c* '((-5 (#xFB))(0 (0))(#x-80 (#x80))(#x-7F (#x81))(#x7F (#x7F))(#x-81 (#xFF #x7F))))

(deftest make-2c ()
  (dolist (specs *specs-2c*)
    (is (egal? (word->bytes (first specs)) (second specs)))))

(deftest parse-2c ()
  (dolist (specs *specs-2c*)
    (is (egal? (signed-integer (make-instance 'word :bytes (second specs))) (first specs)))))


(in-suite all)
(defsuite* parsing)

(deftest read-primitives ()
  (dolist (spec *primitives*)
	(is (egal? (first spec)
			   (first (resolve-words (read-bulk-seq (second spec))))))))

(deftest read-nesting ()
  (is (egal? *nesting* (read-bulk-seq *nesting-bulk*))))

(deftest read-references ()
  (is (egal? *references* (read-bulk-seq *references-bulk*))))

(in-suite all)
(defsuite* writing)

(deftest write-primitives ()
  (dolist (spec *primitives*)
	(is (egal? (second spec)
			   (with-output-to-sequence (out)
				 (write-bulk out (first spec)))))))

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
  (let ((env-bar (make-instance 'compound-lexical-environment :normal *core-1.0* :policy (policy/ns '(:foo :bar))))
		(env-quux (make-instance 'compound-lexical-environment :normal *core-1.0* :policy (policy/ns '(:foo :quux)))))
	(copy/assign! env-bar (lex-value '(:foo :bar) 0) 0)
	(copy/assign! env-bar (lex-value '(:foo :bar) 1) 1)
	(copy/assign! env-bar (lex-value '(:foo :quux) 0) 10)
	(is (= 0 (get-value env-bar (lex-value '(:foo :bar) 0))))
	(is (= 1 (get-value env-bar (lex-value '(:foo :bar) 1))))
	(is (= 10 (get-value env-bar (lex-value '(:foo :quux) 0))))
	(copy/assign! env-quux (lex-value '(:foo :bar) 0) 100)
	(copy/assign! env-quux (lex-value '(:foo :bar) 1) 101)
	(copy/assign! env-quux (lex-value '(:foo :quux) 0) 20)
	(apply-env! env-bar env-quux)
	(is (= 0 (get-value env-bar (lex-value '(:foo :bar) 0))))
	(is (= 1 (get-value env-bar (lex-value '(:foo :bar) 1))))
	(is (= 20 (get-value env-bar (lex-value '(:foo :quux) 0))))))


(defsuite* core)

(defparameter *foo/baz/quux*
  (let ((env *core-1.0*))
	(copy/add-definition! env (make-ns '(:foo 1)
  								(name 0 :value "quux")
								(name 1 :value 42)
  								(name 2 :semantic ({eager} (x) (list :foo x)))
								(name 3 :semantic (fun->eager #'+))
								(name 4 :semantic (fun->eager #'append))
								(name 5 :semantic (fun->lazy #'append)))
  						  :num 40)
	(copy/add-definition! env (make-ns '(:foo 2)
								(name 0 :value (fun->eager #'-))))
	(copy/add-definition! env (make-ns '(:baz 1)
								(name 0 :value (fun->eager #'*))
								(name 1 :value ({eager} (x) (list :baz x)))))
	(copy/add-definition! env (make-pkg '(:quux 0)
										(make-ns '(:quux 1)
										  (name 0 :value 101))
										(make-ns '(:quux 2)
										  (name 0 :semantic ({eager} (x) (list :quux x))))
										(make-ns '(:quux 3)
										  (name 0 :value '(1)))))
	env))

(deftest defines ()
  (is (egal? '(1 2 3 4 10) (eval-whole `((,(ref 32 9) ,(ref 40 10) 1)
										 (,(ref 32 9) ,(ref 40 20) 2)
										 (,(ref 32 9) ,(ref 40 30) 3)
										 (,(ref 32 9) ,(ref 40 40) 4)
										 (,(ref 40 3)
										   (,(ref 40 3) ,(ref 40 10) ,(ref 40 20))
										   (,(ref 40 3) ,(ref 40 30) ,(ref 40 40))))
									   *foo/baz/quux*))))

(deftest namespaces ()
  (let ((env *foo/baz/quux*))
	;; test a NS already associated to a number
	(is (equal "quux" (eval (ref 40 0) env)))
	(labels ((appender (op)
			   (list (ref 40 op) (list (list (ref 40 3) 1 2)) (list 3))))
	  (is (egal? (eval (appender 4) env) (list 3 3)))
	  (is (egal? (eval (appender 5) env) (list (list (qref '(:foo 1) 3) 1 2) 3))))

	;; test a NS identified with a form already known
	(is (equal '(3) (eval-whole (list (list (ref 32 6) 41 (list (ref 40 2) 2))
									  (list (ref 41 0) 4 1))
								env)))

	;; test a self-identifying NS
	(is (equal '(8) (eval-whole (list (list (ref 32 6) 42 (list (ref 42 1) 1))
									  (list (ref 42 0) 2 4))
								env)))

	;; test self-identifying PKG
	(is (equal '(101) (eval-whole (list (list (ref 32 8) 41 2 (list (ref 42 0) 0))
										(ref 41 0))
								  env)))))
;; TODO: test that badly self-identified NS fails

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

(deftest definitions ()
  (is (same? `((42 42) ,(qref '(:std :core) 255))
			 (eval-whole `(((,(ref 32 9) ,(ref 32 255) 42) ,(ref 32 255)) ,(ref 32 255)) *core-1.0*))))

(defparameter *pies* '(3.141592653589793d0 3.1415927f0))
(defparameter *pies-bulk* #(1 32 34 7 64 9 33 251 84 68 45 24 2 1 32 34 6 64 73 15 219 2))

(deftest arithmetic ()
  (is (= (/ 3 8) (eval (list (ref #x20 #x20) 3 8) *core-1.0*)))
  (is (= -1 (eval (list (ref #x20 #x21) (word #xFF #xFF)) *core-1.0*)))
  (is (egal? *pies-bulk* (with-output-to-sequence (out) (write-whole out *pies*))))
  (is (egal? *pies* (eval-whole (read-bulk-seq *pies-bulk*) *core-1.0*))))

(deftest concat ()
  (is (equal '(8 9 10 11) (coerce (get-bytes (eval (first (read-bulk-seq #(1 32 16 3 4 2 8 9 3 4 2 10 11 2))) *core-1.0*)) 'list))))
