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

(uiop:define-package :bulk/vocabularies/lambda
  (:use :cl :bulk/write :bulk/reference :bulk/write/verifiable)
  (:export #:main #:lambda-ns))

(in-package :bulk/vocabularies/lambda)

(defun lambda-ns (&optional (stream (make-broadcast-stream)))
  (write-verifiable-ns (stream (:shake128 :output-length 8) #x29 (ref #x28 #x21) "λ"
							   "This vocabulary can be used to represent functions that can be evaluated."
							   :counter counter)
	(write-bulk -> `(,(ref #x20 #xA) ,(ref #x29 counter) "lambda" "( lambda {var}:Ref {body} ) ; type LazyFunction"))
	(write-bulk -> `(,(ref #x20 #x9) ,(ref #x29 #xFF) "This reference is intended to be used as lambda function variable."))
	(map nil (lambda (char)
			   (write-bulk -> (list (ref #x20 #xA) (ref #x29 counter) (string char) (ref #x29 #xFF))))
		 "abcdefghijklmnopqrstuvwxyz")))


(defun main (&optional (path "lambda.bulk"))
  (with-open-file (out path :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
	(write-bulk out (list (ref #x20 #x0) 1 0))
	(write-bulk out (list (ref #x20 #x6) #x28 (list (ref #x28 #x21) #x97102AB94C55DDF0)))
	(lambda-ns out)))
