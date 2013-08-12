 #| BULK library
    Copyright (C) 2013 Pierre Thierry <pierre@nothos.net>

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

(defpackage :nothos.net/2013.08.bulk-test
  (:use :cl :hu.dwim.stefil :bulk)
  (:export #:all #:maths #:parsing)
  (:nicknames :bulk-test))

(in-package :nothos.net/2013.08.bulk-test)

(defparameter *primitives* '((:nil #(#x48 #x65 #x6C #x6C #x6F #x20
#x77 #x6F #x72 #x6C #x64 #x21) #x2A #x100 #x1000000 #x123456789ABCDEF
#x-80)))
(defparameter *nesting* '(:nil nil (nil nil)))
(defparameter *references* (list (ref #x10 #x1)
				 (ref #x10 #x2)
				 (ref #xFE #xFF)
				 (ref #xFFFFBC #x80)))

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
  (is (equalp *primitives* (rest (read-file (asdf:system-relative-pathname "bulk" "tests/primitives.bulk"))))))

(deftest read-nesting ()
  (is (equalp *nesting* (rest (read-file (asdf:system-relative-pathname "bulk" "tests/nesting.bulk"))))))

(deftest read-references ()
  (is (equalp *references* (rest (read-file (asdf:system-relative-pathname "bulk" "tests/references.bulk"))))))
