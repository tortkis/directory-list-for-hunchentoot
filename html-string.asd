
(in-package :cl-user)

(defpackage #:html-string-asd
  (:use :cl :asdf))

(in-package :html-string-asd)

(defsystem html-string
  :name "html-string"
  :version "0.1"
  :depends-on ()
  :serial t
  :components ((:file "html-string")))
