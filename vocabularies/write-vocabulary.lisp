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


(uiop:define-package :bulk/vocabularies/write-vocabulary
	(:use :cl)
  (:export #:main))

(in-package :bulk/vocabularies/write-vocabulary)

(defun main ()
  (let* ((name (first (uiop:command-line-arguments)))
		(system (intern (string-upcase (format nil "BULK/VOCABULARIES/~a" name)) :keyword)))
	(ql:quickload system)
	(let ((func (symbol-function (intern "MAIN" system))))
	  (funcall func)
	  (uiop:quit))))


#|
(ql:quickload "bulk/vocabularies/write-vocabulary")
(save-lisp-and-die "write-vocabulary" :executable t :toplevel #'bulk/vocabularies/write-vocabulary:main)
|#
