
(in-package :cl-user)

(defpackage #:directory-list-asd
  (:use :cl :asdf))

(in-package :directory-list-asd)

(defsystem directory-list
  :name "directory-list"
  :version "0.1"
  :depends-on (:hunchentoot
               :html-string)
  :serial t
  :components ((:file "directory-list")))
  