(in-package #:bluespec)

(defun toctree (spec docs)
  "Create the TOC tree from a spec-page."
  (let ((fstr (make-array '(0) :element-type 'base-char
                          :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s fstr)
      ;; Index name
      (let* ((title (remove-index (title spec)))
             (underline (make-string (length title) :initial-element #\*)))
        (format s "~a~%~a~%~a~%~%" underline title underline))

      (format s "
.. toctree::
   :maxdepth: 2
   :numbered:

")

      ;; Index contents
      (let ((index (read-title-index (title spec))))
        (loop for (page fname) in docs do
             (let ((page-index (read-title-index (title page))))
               (when (and page-index
                          (= 1 (- (length page-index) (length index)))
                          (equalp (subseq page-index 0 (length index))
                                  index))
                 (format s "   ~a~%" (subseq fname 0 (- (length fname) 4)))))))

      fstr)))


(defun print-docs (docs &optional (path "./"))
  "Output the document contents to a directory."
  (with-open-file (index (format NIL "~a/index.rst" path) :direction :output)
    (format index "Welcome to BlueSpec's documentation!
====================================

Contents:

.. toctree::
   :maxdepth: 2
   :numbered:

")
    (loop for (spec fname) in docs do
         (let ((oname (format NIL "~a/~a.rst"
                              path
                              (subseq fname 0 (- (length fname) 4))))
               (depth (length (read-title-index (title spec)))))
           ;; (format t "~a -> ~a~%" fname oname)
           (with-open-file (f oname :direction :output)
             (if (or (= depth 1)
                     (toctreep spec))
                 (progn (when (= depth 1)
                          (format index "   ~a~%"
                                   (subseq fname 0 (- (length fname) 4))))
                        (format f "~a" (strip (toctree spec docs))))
                 (progn (format f "~a" (strip (rst spec)))
                        (format f "~%~%")
                        (loop for key being the hash-keys in (link-table spec)
                           using (hash-value value) do
                             (format f ".. _~a: ~a~%"
                                     (regex-replace-all ":" key "\\:")
                                     value)))))))))
