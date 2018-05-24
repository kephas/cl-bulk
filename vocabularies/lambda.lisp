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
  (:use :cl :bulk/write :bulk/reference))

(in-package :bulk/vocabularies/lambda)

(defun main (&optional (path "lambda.bulk"))
  (with-open-file (out path :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
	(write-whole
	 out
	 `((,(ref #x20 #x0) 1 0)
	   (,(ref #x20 #x6) #x28 (,(ref #x28 #x1A) ,(expt 2 56)))
	   (,(ref #x20 #xC) #x29 (,(ref #x28 #x1A) ,(expt 2 56)) "λ"
		 "This vocabulary can be used to represent functions that can be evaluated."

		 (,(ref #x20 #xA) :nil "lambda" "( lambda {var}:Ref {body} ) ; type LazyFunction")
		 (,(ref #x20 #x9) ,(ref #x29 #xFF) "This reference is intended to be used as lambda function variable.")

		 (,(ref #x20 #xA) :nil "a" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "b" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "c" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "d" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "e" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "f" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "g" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "h" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "i" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "j" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "k" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "l" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "m" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "n" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "o" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "p" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "q" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "r" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "s" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "t" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "u" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "v" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "w" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "x" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "y" ,(ref #x29 #xFF))
		 (,(ref #x20 #xA) :nil "z" ,(ref #x29 #xFF)))))))
