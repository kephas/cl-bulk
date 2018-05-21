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

(uiop:define-package :bulk/reference
  (:use :cl :alexandria)
  (:export #:ref #:ns #:name #:ref-constructor))

(in-package :bulk/reference)

(defclass ref ()
  ((ns :initarg :ns)
   (name :initarg :name)))

(defun ref (ns name)
  (make-instance 'ref :ns ns :name name))


(defgeneric ref-constructor (ref)
  (:documentation "Return the function used to construct a reference"))

(defmethod ref-constructor (ref)
  nil)

(defmethod ref-constructor ((ref ref))
  'ref)


(defmethod print-object ((object ref) stream)
  (with-slots (ns name) object
	(if-let (constructor (ref-constructor object))
	  (format stream "(~a ~a ~a)" constructor ns name)
	  (print-unreadable-object (object stream :type t :identity t)
		(format stream "~a ~a" ns name)))))
