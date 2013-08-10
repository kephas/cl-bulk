(defpackage :nothos.net/2013.08.bulk-system
  (:use :common-lisp :asdf))

(in-package :nothos.net/2013.08.bulk-system)

(defsystem "bulk"
  :description "BULK library"
  :version "0.1.0"
  :author "Pierre Thierry <pierre@nothos.net>"
  :licence "AGPL"
  :depends-on ("thierry-macros")
  :components ((:file "package")
	       (:file "read")
	       (:file "write"))
  :serial t)
