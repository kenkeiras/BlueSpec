;;; BlueSpec converter
;;; Package definition

(in-package #:cl-user)

(defpackage #:bluespec
  (:use :cl :html5-parser :cl-fad :cl-ppcre)
  (:export #:spec-page #:print-docs #:read-docs))

(in-package #:bluespec)

(defparameter *UNDERLINES* "=-;:_.,\"'")
