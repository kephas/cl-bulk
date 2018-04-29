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

(in-package :nothos.net/2013.08.bulk)

(defgeneric write-bulk (stream bulk)
  (:documentation "Serialize a BULK expression to a binary stream."))


(define-condition serialization-error (error) ())
(define-condition unimplemented-serialization (serialization-error) ())

(defmethod write-bulk (stream bulk)
  (error 'unimplemented-serialization))


(defmethod write-bulk (stream (bulk (eql :nil)))
  (declare (ignore bulk))
  (write-byte 0 stream))

(defmethod write-bulk (stream (bulk list))
  (write-byte 1 stream)
  (dolist (expr bulk)
    (write-bulk stream expr))
  (write-byte 2 stream))

(defmethod write-bulk (stream (bulk vector))
  (write-byte 3 stream)
  (write-bulk stream (length bulk))
  (write-sequence bulk stream))


(defun %write-unsigned-payload (stream value bytes)
  (let@ rec ((count bytes))
    (unless (zerop count)
      (write-byte (ldb (byte 8 (* 8 (1- count))) value) stream)
      (rec (1- count)))))

(defun make-2c-notation (value bytes)
  (let ((msb<<1 (ash 1 (* 8 bytes))))
    (mod (+ msb<<1 value) msb<<1)))

(defmethod write-bulk (stream (bulk integer))
  (typecase bulk
    ((integer 0 #xFF)
     (progn (write-byte 4 stream)
	    (%write-unsigned-payload stream bulk 1)))
    ((integer 0 #xFFFF)
     (progn (write-byte 5 stream)
	    (%write-unsigned-payload stream bulk 2)))
    ((integer 0 #xFFFFFFFF)
     (progn (write-byte 6 stream)
	    (%write-unsigned-payload stream bulk 4)))
    ((integer 0 #xFFFFFFFFFFFFFFFF)
     (progn (write-byte 7 stream)
	    (%write-unsigned-payload stream bulk 8)))
    ((integer 0 #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
     (progn (write-byte 8 stream)
	    (%write-unsigned-payload stream bulk 16)))
    ((integer #x-FF 0)
     (progn (write-byte 9 stream)
	    (%write-unsigned-payload stream (- bulk) 1)))
    ((integer #x-FFFF 0)
     (progn (write-byte 10 stream)
	    (%write-unsigned-payload stream (- bulk) 2)))
    ((integer #x-FFFFFFFF 0)
     (progn (write-byte 11 stream)
	    (%write-unsigned-payload stream (- bulk) 4)))
    ((integer #x-FFFFFFFFFFFFFFFF 0)
     (progn (write-byte 12 stream)
	    (%write-unsigned-payload stream (- bulk) 8)))
    ((integer #x-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF 0)
     (progn (write-byte 13 stream)
	    (%write-unsigned-payload stream (- bulk) 16)))
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

