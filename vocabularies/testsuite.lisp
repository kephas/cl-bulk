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

(uiop:define-package :bulk/vocabularies/testsuite
  (:use :cl :bulk/write :bulk/reference :bulk/words))

(in-package :bulk/vocabularies/testsuite)

(defun main (&optional (path "testsuite.bulk"))
  (with-open-file (out path :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
	(write-whole
	 out
	 `((,(ref #x20 #x0) 1 0)
	   (()(())(()())(:nil)(:nil(:nil):nil))
	   "Hello World!"
	   "From Tōkyō to México"
	   (,@(list 1 (expt 2 8) (expt 2 24) (expt 2 56) (expt 2 120)) ,@(mapcar #'- (list 1 (expt 2 8) (expt 2 24) (expt 2 56) (expt 2 120))))
	   ("A000045" 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040 1346269 2178309 3524578 5702887 9227465 14930352 24157817 39088169 63245986 102334155)
	   ("A021009" 1 1 -1 2 -4 1 6 -18 9 -1 24 -96 72 -16 1 120 -600 600 -200 25 -1 720 -4320 5400 -2400 450 -36 1 5040 -35280 52920 -29400 7350 -882 49 -1 40320 -322560 564480 -376320 117600 -18816 1568 -64 1 362880 -3265920)
	   ("π" 3.141592653589793d0 3.1415927f0)
	   ("googol" ,(expt 10 100))))))
