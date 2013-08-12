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

(defpackage :nothos.net/2013.08.bulk
  (:use :cl :scheme)
  (:export #:read-unsigned-word #:ref
	   #:read-bulk #:read-whole #:read-file
	   #:parse-2c-notation #:make-2c-notation
	   #:write-bulk #:create-bulk-file #:append-to-bulk-file)
  (:nicknames :bulk))
