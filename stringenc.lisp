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

(uiop:define-package :bulk/stringenc
  (:use :cl)
  (:export #:iana-charset-mib->babel-name #:windows-code-page->babel-name))

(in-package :bulk/stringenc)

(defun iana-charset-mib->babel-name (mib-enum)
  (case mib-enum
	(3 :us-ascii)
	(4 :iso-8859-1)
	(5 :iso-8859-2)
	(6 :iso-8859-3)
	(7 :iso-8859-4)
	(8 :iso-8859-5)
	(9 :iso-8859-6)
	(10 :iso-8859-7)
	(11 :iso-8859-8)
	(12 :iso-8859-9)
	(13 :iso-8859-10)
	(109 :iso-8859-13)
	(110 :iso-8859-14)
	(111 :iso-8859-15)
	(112 :iso-8859-16)
	(2259 :iso-8859-11)
	(106 :utf-8)
	(1013 :utf-16be)
	(1014 :utf-16le)
	(1015 :utf-16)
	(1017 :utf-32)
	(1018 :utf-32be)
	(1019 :utf-32le)
	(18 :eucjp)
	(2028 :ebcdic-us)
	(2029 :ebcdic-international)
	(2251 :windows-1251)
	(2252 :windows-1252)))
								 

(defun windows-code-page->babel-name (cp)
  (case cp
	(37 :ebcdic-us)
	(500 :ebcdic-international)
	(1200 :utf-16le)
	(1201 :utf-16be)
	(1251 :windows-1251)
	(1252 :windows-1252)
	(12000 :utf-32)
	(120001 :utf-32be)
	(20127 :us-ascii)
	(28591 :iso-8859-1)
	(28592 :iso-8859-2)
	(28593 :iso-8859-3)
	(28594 :iso-8859-4)
	(28595 :iso-8859-5)
	(28596 :iso-8859-6)
	(28597 :iso-8859-7)
	(28598 :iso-8859-8)
	(28599 :iso-8859-9)
	(28603 :iso-8859-13)
	(28605 :iso-8859-15)
	(51932 :eucjp)
	(65001 :utf-8)))
