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

(uiop:define-package :bulk/read
  (:use :cl :bulk/reference :bulk/words :scheme :alexandria :flexi-streams)
  (:export #:read-bulk #:read-whole #:read-file
		   #:parsing-error)
  (:reexport :bulk/reference))

(in-package :bulk/read)

(defun %read-form-payload (stream top-level?)
  (let@ rec ((expressions)
	     (next (%read-bulk stream top-level?)))
    (if (eq next :end)
	(reverse expressions)
	(rec (cons next expressions) (%read-bulk stream top-level?)))))

(defun %read-array-payload (stream)
  (let* ((size (unsigned-integer (read-bulk stream)))
		 (array (make-array size :element-type '(unsigned-byte 8))))
    (read-sequence array stream)
    array))

(defun read-unsigned-word (stream size)
  "Read the next SIZE bytes in STREAM as a big-endian unsigned
integer"
  (let ((bytes (make-array size)))
	(read-sequence bytes stream)
	(make-instance 'word :bytes bytes)))

(defun %read-ref-payload (stream marker)
  (let* ((ns (if (eql #xFF marker)
		 (let@ rec ((ns #xFF)
			    (next (read-byte stream)))
		   (let ((ns (+ ns next)))
		     (if (eql #xFF next)
			 (rec ns (read-byte stream))
			 ns)))
		 marker))
	 (name (read-byte stream)))
    (ref ns name)))

(define-condition parsing-error (error)
  ((pos :initarg :pos)))

(defun %read-bulk (stream top-level?)
  (let ((marker (read-byte stream (not top-level?) :end)))
    (case marker
      (0 :nil)
      (1 (%read-form-payload stream nil))
      (2 (if top-level? (error 'parsing-error) :end))
      (3 (%read-array-payload stream))
      (4 (read-unsigned-word stream 1))
      (5 (read-unsigned-word stream 2))
      (6 (read-unsigned-word stream 4))
      (7 (read-unsigned-word stream 8))
      (8 (read-unsigned-word stream 16))
      (9 (- (unsigned-integer (read-unsigned-word stream 1))))
      (10 (- (unsigned-integer (read-unsigned-word stream 2))))
      (11 (- (unsigned-integer (read-unsigned-word stream 4))))
      (12 (- (unsigned-integer (read-unsigned-word stream 8))))
      (13 (- (unsigned-integer (read-unsigned-word stream 16))))
      ((14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31) (error 'parsing-error :pos (if-let (pos (file-position stream)) (1- pos))))
      (:end :end)
      (t (%read-ref-payload stream marker)))))

(defun read-bulk (stream)
  "Read one BULK expression from a BULK stream"
  (%read-bulk stream nil))


(define-condition unsupported-bulk-version (error)
  ((version :initarg :version)))

(define-condition unknown-bulk-version (error) ())

(defun %read-whole-stream (stream &key version)
  "Parse a whole BULK stream as a sequence of BULK expressions"
  (if version
	  (if (equal '(1 0) version)
		  (%read-form-payload stream t)
		  (error 'unsupported-bulk-version :version version))
	  (let ((first-form (read-bulk stream)))
		(if (and (listp first-form)
				 (typep (first first-form) 'ref)
				 (with-slots (ns name) (first first-form)
				   (and (eql #x20 ns) (eql 0 name))))
			(let ((version (mapcar #'unsigned-integer (rest first-form))))
			  (if (equal '(1 0) version)
				  (cons first-form (%read-form-payload stream t))
				  (error 'unsupported-bulk-version :version version)))
			(error 'unknown-bulk-version)))))

(defgeneric read-whole (source &key version))

(defmethod read-whole ((source stream) &key version)
  (%read-whole-stream source :version version))

(defmethod read-whole ((source vector) &key version)
  (with-input-from-sequence (in source)
    (%read-whole-stream in :version version)))


(defun read-file (pathspec &key version)
  "Parse a whole BULK file"
  (with-open-file (bulk-stream pathspec :element-type '(unsigned-byte 8))
    (read-whole bulk-stream :version version)))
