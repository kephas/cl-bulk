 #| BULK library
    Copyright (C) 2018 Pierre Thierry <pierre@nothos.net>

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

(uiop:define-package :bulk/words
  (:use :cl :scheme)
  (:export #:parse-2c-notation #:make-2c-notation #:bytes->word #:word->bytes #:word #:bytes
		   #:unsigned-integer #:signed-integer))

(in-package :bulk/words)


(defun parse-2c-notation (value bytes)
  "Parse the integer VALUE as a word of size BYTES in two's complement
notation"
  (let ((msb (ash 1 (1- (* 8 bytes)))))
    (- (boole boole-and value (1- msb))
       (boole boole-and value msb))))

(defun make-2c-notation (value bytes)
  (let ((msb<<1 (ash 1 (* 8 bytes))))
    (mod (+ msb<<1 value) msb<<1)))


(defun bytes->word (bytes)
  (reduce (lambda (word byte) (logior (ash word 8) byte)) bytes))

(defun repeat (value count)
  (let@ rec ((values nil) (count count))
	(if (<= count 0) values (rec (cons value values) (1- count)))))

(defun word->bytes (word &key (byte-size 8) length)
  (let@ rec ((bytes nil)
			 (word word))
	(if (or (eql word 0) (eql word -1))
		(if length
			(last (append (repeat (if (zerop word) 0 255) (- length (length bytes))) bytes) length)
			bytes)
		(rec (cons (ldb (byte byte-size 0) word) bytes) (ash word (- byte-size))))))


(defclass word ()
  ((bytes :initarg :bytes)))

(defun word (&rest bytes)
  (make-instance 'word :bytes bytes))

(defun unsigned-integer (word)
  (bytes->word (slot-value word 'bytes)))

(defun signed-integer (word)
  (with-slots (bytes) word
	(parse-2c-notation (bytes->word bytes) (length bytes))))
