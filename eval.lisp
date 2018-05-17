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

(uiop:define-package :bulk/eval
  (:use :cl :alexandria :bulk/reference)
  (:export #:lexical-environment #:copy/assign #:copy/assign! #:get-value))

(in-package :bulk/eval)


(defclass lexical-environment ()
  ((table :initform (make-hash-table :test 'equal) :initarg :table)))

(defun copy/assign (env field value)
  (let* ((table (copy-hash-table (slot-value env 'table)))
		 (new-env (make-instance 'lexical-environment :table table)))
	(setf (gethash field table) value)
	new-env))

(defmacro copy/assign! (place field value)
  `(setf ,place (copy/assign ,place ,field ,value)))

(defun get-value (env field)
  (gethash field (slot-value env 'table)))
