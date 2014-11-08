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

(defun %read-form-payload (stream top-level?)
  (let@ rec ((expressions)
	     (next (%read-bulk stream top-level?)))
    (if (eq next :end)
	(reverse expressions)
	(rec (cons next expressions) (%read-bulk stream top-level?)))))

(defun %read-array-payload (stream)
  (let* ((size (read-bulk stream))
	 (array (make-array size :element-type '(unsigned-byte 8))))
    (read-sequence array stream)
    array))

(defun read-unsigned-word (stream bytes)
  "Read the next BYTES bytes in STREAM as a big-endian unsigned
integer"
  (let@ rec ((count bytes)
	     (value 0))
    (if (zerop count)
	value
	(progn
	  (setf (ldb (byte 8 (* 8 (1- count))) value) (read-byte stream))
	  (rec (1- count) value)))))

(defun parse-2c-notation (value bytes)
  "Parse the integer VALUE as a word of size BYTES in two's complement
notation"
  (let ((msb (ash 1 (1- (* 8 bytes)))))
    (- (boole boole-and value (1- msb))
       (boole boole-and value msb))))

(defun %read-signed-payload (stream)
  (multiple-value-bind (value bytes) (read-bulk stream)
    (parse-2c-notation value bytes)))

(defun %read-ref-payload (stream marker)
  (let* ((ns (if (eql #xFF marker)
		 (let@ rec ((ns #xFF00)
			    (next (read-byte stream)))
		   (let ((ns (+ ns next)))
		     (if (eql #xFF next)
			 (rec (ash ns 8) (read-byte stream))
			 ns)))
		 marker))
	 (name (read-byte stream)))
    (ref ns name)))

(define-condition parsing-error (error) ())

(defun %read-bulk (stream top-level?)
  (let ((marker (read-byte stream (not top-level?) :end)))
    (case marker
      (0 :nil)
      (1 (%read-form-payload stream nil))
      (2 (if top-level? (error 'parsing-error) :end))
      (3 (%read-array-payload stream))
      (4 (values (read-unsigned-word stream 1) 1))
      (5 (values (read-unsigned-word stream 2) 2))
      (6 (values (read-unsigned-word stream 4) 4))
      (7 (values (read-unsigned-word stream 8) 8))
      (8 (values (read-unsigned-word stream 16) 16))
      (9 (%read-signed-payload stream))
      ((10 11 12 13 14 15) (error 'parsing-error))
      (:end :end)
      (t (%read-ref-payload stream marker)))))

(defun read-bulk (stream)
  "Read one BULK expression from a BULK stream"
  (%read-bulk stream nil))

(defun read-whole (stream)
  "Parse a whole BULK stream as a sequence of BULK expressions"
  (%read-form-payload stream t))

(defun read-file (pathspec)
  "Parse a whole BULK file"
  (with-open-file (bulk-stream pathspec :element-type '(unsigned-byte 8))
    (read-whole bulk-stream)))
