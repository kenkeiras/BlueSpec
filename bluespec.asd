;;; BlueSpec converter
;;; ASDF system definition

(defpackage #:bluespec-system
  (:use :asdf :cl))

(in-package #:bluespec-system)

(ql:quickload 'cl-html5-parser)
(ql:quickload 'cl-fad)
(ql:quickload 'cl-ppcre)

(asdf:defsystem #:bluespec
  :name "bluespec"
  :description "A HyperSpec's HTML to Sphinx's RST conversor."
  :licence "WTFPLv2"
  :serial t
  :depends-on (:cl-html5-parser :cl-fad :cl-ppcre)
  :components
  ((:file "package")
   (:file "utilities" :depends-on ("package"))
   (:file "parse-tree" :depends-on ("package" "utilities"))
   (:file "spec-page" :depends-on ("package" "utilities" "parse-tree"))
   (:file "reader" :depends-on ("package"))
   (:file "writer" :depends-on ("package" "utilities"))))
