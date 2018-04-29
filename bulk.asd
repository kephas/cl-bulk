(defpackage :nothos.net/2013.08.bulk-system
  (:use :common-lisp :asdf))

(in-package :nothos.net/2013.08.bulk-system)

(defsystem "bulk"
  :description "BULK library"
  :version "0.1"
  :author "Pierre Thierry <pierre@nothos.net>"
  :licence "AGPL"
  :depends-on ("hu.dwim.stefil" "scheme" "flexi-streams" "trivial-utf-8")
  :components ((:file "package")
	       (:file "reference")
	       (:file "read")
	       (:file "write")
	       (:file "test"))
  :serial t)
