 #| BULK library
    Copyright (C) 2013--2019 Pierre Thierry <pierre@nothos.net>

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

(uiop:define-package :bulk/spec
	(:use :cl)
  (:import-from :cl-annot))

(annot:enable-annot-syntax)

(in-package :bulk/spec)

@export
(defconstant +bulk/+ #x20)

@export
(defconstant +bulk/version+ #x00)

@export
(defconstant +bulk/true+ #x01)

@export
(defconstant +bulk/false+ #x02)

@export
(defconstant +bulk/stringenc+ #x03)

@export
(defconstant +bulk/iana-charset+ #x04)

@export
(defconstant +bulk/codepage+ #x05)

@export
(defconstant +bulk/ns+ #x06)

@export
(defconstant +bulk/package+ #x07)

@export
(defconstant +bulk/import+ #x08)

@export
(defconstant +bulk/define+ #x09)

@export
(defconstant +bulk/mnemonic/def+ #x0A)

@export
(defconstant +bulk/ns-mnemonic+ #x0B)

@export
(defconstant +bulk/verifiable-ns+ #x0C)


@export
(defconstant +bulk/concat+ #x10)

@export
(defconstant +bulk/subst+ #x11)

@export
(defconstant +bulk/arg+ #x12)

@export
(defconstant +bulk/rest+ #x13)


@export
(defconstant +bulk/frac+ #x20)

@export
(defconstant +bulk/bigint+ #x21)

@export
(defconstant +bulk/binary+ #x22)

@export
(defconstant +bulk/decimal+ #x23)


@export
(defconstant +bulk/prefix-bytecode+ #x30)

@export
(defconstant +bulk/prefix-bytecode*+ #x31)

@export
(defconstant +bulk/postfix-bytecode+ #x32)

@export
(defconstant +bulk/postfix-bytecode*+ #x33)

@export
(defconstant +bulk/arity+ #x34)

@export
(defconstant +bulk/property-list+ #x35)
