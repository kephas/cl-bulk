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

(defmethod write-bulk (stream (bulk (eql :nil)))
  (declare (ignore bulk))
  (write-byte 0 stream))

(defmethod write-bulk (stream (bulk list))
  (write-byte 1 stream)
  (dolist (expr bulk)
    (write-bulk stream expr))
  (write-byte 2 stream))


#| Function to write BULK data to a file |#

(defun %write-bulk-to-file (pathspec bulk-list mode)
  (with-open-file (out pathspec :element-type '(unsigned-byte 8)
		       :direction :output :if-exists mode :if-does-not-exist :create)
    (dolist (expr bulk-list)
      (write-bulk out expr))))

(defun create-bulk-file (pathspec bulk-list)
  "Create a new BULK stream in the file designated by {pathspec} with
expressions {bulk-list}. If the file already exists, it is
overwritten."
  (%write-bulk-to-file pathspec bulk-list :supersede))

(defun append-to-bulk-file (pathspec bulk-list)
  "Append expressions {bulk-list} to the BULK stream in the file
designated by {pathspec}."
  (%write-bulk-to-file pathspec bulk-list :append))

