(in-package #:bluespec)

(defun parse (fname)
  "Read a file and return it's node-based representation."
  (with-open-file (f fname)
    (let ((text (make-string (file-length f))))
      (read-sequence text f)
      (node-to-xmls (parse-html5-fragment text) NIL))))


(defun parse-page (&optional (fname "Front/Contents.htm"))
  "Convert a file into a spec-page."
  (let ((front-content (parse fname)))
    (make-instance 'spec-page
                   :content front-content)))

(defun read-docs (&optional (path "./"))
  "Parse the documents in the Body directory."
  (loop for f in (list-directory (merge-pathnames path "Body"))
     collect (let ((fname (file-namestring f))
                   (page (parse-page f)))
               (list page fname))))
