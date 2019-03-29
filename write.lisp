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

(uiop:define-package :bulk/write
  (:use :cl :bulk/reference :bulk/words :bulk/spec :scheme :trivial-utf-8 :ieee-floats)
  (:export #:write-bulk #:write-whole
		   #:arbitrary-bytes
		   #:create-bulk-file #:append-to-bulk-file
		   #:unimplemented-serialization))

(in-package :bulk/write)

(defgeneric write-bulk (stream bulk)
  (:documentation "Serialize a BULK expression to a binary stream."))


(define-condition serialization-error (error) ())
(define-condition unimplemented-serialization (serialization-error) ())

(defmethod write-bulk (stream bulk)
  (error 'unimplemented-serialization))


(defclass arbitrary-bytes ()
  ((bytes :initarg :bytes)))

(defun arbitrary-bytes (bytes)
  (make-instance 'arbitrary-bytes :bytes bytes))

(defmethod write-bulk (stream (bulk arbitrary-bytes))
  (write-sequence (slot-value bulk 'bytes) stream))


(defmethod write-bulk (stream (bulk (eql :nil)))
  (declare (ignore bulk))
  (write-byte 0 stream))

(defmethod write-bulk (stream (bulk list))
  (write-byte 1 stream)
  (dolist (expr bulk)
    (write-bulk stream expr))
  (write-byte 2 stream))

(defmethod write-bulk (stream (bulk vector))
  (typecase bulk
	((vector (unsigned-byte 8)) (progn
								  (write-byte 3 stream)
								  (write-bulk stream (length bulk))
								  (write-sequence bulk stream)))
	((vector character) (write-bulk stream (trivial-utf-8:string-to-utf-8-bytes bulk)))
	(t (let ((bytes (handler-case (coerce bulk '(vector (unsigned-byte 8)))
					  (error () (error 'unimplemented-serialization)))))
		 (write-bulk stream bytes)))))

(defun %write-word (stream bytes &optional negative?)
  (let* ((length (length bytes))
		 (marker (cadr (assoc length '((1 4)(2 5)(4 6)(8 7)(16 8))))))
	(if marker
		(progn
		  (write-byte (if negative? (+ 5 marker) marker) stream)
		  (write-sequence bytes stream))
		(write-bulk stream (coerce bytes 'vector)))))

(defmethod write-bulk (stream (bulk word))
  (%write-word stream (get-bytes bulk)))

(defmethod write-bulk (stream (bulk integer))
  (typecase bulk
    ((integer 0 #xFF) (%write-word stream (word->bytes bulk :length 1)))
    ((integer 0 #xFFFF) (%write-word stream (word->bytes bulk :length 2)))
    ((integer 0 #xFFFFFFFF) (%write-word stream (word->bytes bulk :length 4)))
    ((integer 0 #xFFFFFFFFFFFFFFFF) (%write-word stream (word->bytes bulk :length 8)))
    ((integer 0 #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF) (%write-word stream (word->bytes bulk :length 16)))
    ((integer #x-FF 0) (%write-word stream (word->bytes (- bulk) :length 1) t))
    ((integer #x-FFFF 0) (%write-word stream (word->bytes (- bulk) :length 2) t))
    ((integer #x-FFFFFFFF 0) (%write-word stream (word->bytes (- bulk) :length 4) t))
    ((integer #x-FFFFFFFFFFFFFFFF 0) (%write-word stream (word->bytes (- bulk) :length 8) t))
    ((integer #x-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF 0) (%write-word stream (word->bytes (- bulk) :length 16) t))
    (t (write-bulk stream
				   (list (ref +bulk/+ +bulk/bigint+) (coerce (word->bytes bulk :twoc t) '(vector (unsigned-byte 8))))))))

(defmethod write-bulk (stream (bulk ratio))
  (write-bulk stream (list (ref +bulk/+ +bulk/frac+) (numerator bulk) (denominator bulk))))

(defmethod write-bulk (stream (bulk float))
  (cond
	((<= (float-precision bulk) 24)
	 (write-bulk stream (list (ref +bulk/+ +bulk/binary+)
							  (make-instance 'word :bytes (word->bytes (encode-float32 bulk) :length 4)))))
	((<= (float-precision bulk) 53)
	 (write-bulk stream (list (ref +bulk/+ +bulk/binary+)
							  (make-instance 'word :bytes (word->bytes (encode-float64 bulk) :length 8)))))
	(t (error 'unimplemented-serialization))))


(defmethod write-bulk (stream (bulk ref))
  (with-slots (ns name) bulk
    (multiple-value-bind (quot rem) (truncate ns 255)
      (dotimes (n quot) (write-byte 255 stream))
      (write-byte rem stream))
    (write-byte name stream)))

#| Function to write BULK data to a file |#

(defun write-whole (stream bulk-list)
  (dolist (expr bulk-list)
    (write-bulk stream expr)))

(defun %write-bulk-to-file (pathspec bulk-list mode)
  (with-open-file (out pathspec :element-type '(unsigned-byte 8)
		       :direction :output :if-exists mode :if-does-not-exist :create)
    (write-whole out bulk-list)))

(defun create-bulk-file (pathspec bulk-list)
  "Create a new BULK stream in the file designated by {pathspec} with
expressions {bulk-list}. If the file already exists, it is
overwritten."
  (%write-bulk-to-file pathspec bulk-list :supersede))

(defun append-to-bulk-file (pathspec bulk-list)
  "Append expressions {bulk-list} to the BULK stream in the file
designated by {pathspec}."
  (%write-bulk-to-file pathspec bulk-list :append))

