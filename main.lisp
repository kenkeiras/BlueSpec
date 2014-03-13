(load "~/.sbclrc")
(asdf:load-system 'bluespec)

(in-package #:bluespec)

(defun main (input-path output-path)
  "Read HyperSpec and write out the processed output."
  (let ((docs (read-docs input-path)))
    (format t "~a documents ~%" (length docs))
    (print-docs docs output-path)))

(main "HyperSpec/" "result/")
