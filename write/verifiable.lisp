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

(uiop:define-package :bulk/write/verifiable
  (:use :cl :alexandria :bulk/write :bulk/reference :bulk/words :flexi-streams)
  (:import-from :ironclad)
  (:export #:write-verifiable-ns #:->))

(in-package :bulk/write/verifiable)

(defmacro write-verifiable-ns ((stream (&rest hash-params) num hash-ref name doc &key counter) &body body)
  (with-gensyms (counter-var content)
  `(let* (,@(if counter `((,counter-var -1)))
		  (,content
		   (symbol-macrolet (,@(if counter `((,counter (incf ,counter-var)))))
			 (with-output-to-sequence (->)
			   (write-bulk -> ,name)
			   (write-bulk -> ,doc)
			   ,@body)))
			(hash (word* (ironclad:digest-sequence (ironclad:make-digest ,@hash-params) ,content))))
	 (write-bulk ,stream (list (ref #x20 #xC) ,num (list ,hash-ref hash) (arbitrary-bytes ,content)))
	 hash)))
