(ql:quickload "cl-html5-parser")
(ql:quickload "cl-fad")
(ql:quickload "cl-ppcre")


(defpackage #:bluespec
  (:use :cl :html5-parser :cl-fad :cl-ppcre)
  (:export #:spec-page #:print-docs #:read-docs #:reload-docs))

(in-package #:bluespec)


(defparameter *UNDERLINES* "=-;:_.,\"'")

(defclass spec-page()
  ((title :accessor title)
   (rst :accessor rst)
   (raw :accessor raw)
   (toctreep :accessor toctreep)
   (link-table :accessor link-table)))




(defmethod initialize-instance :after ((page spec-page)
                                       &key ((:content content)))
  (setf (raw page) content)
  (setf (link-table page) (make-hash-table :test #'equalp))

  (let ((title (get-text (car (get-in-toplevel content "h2")))))
    (setf (title page)
          (strip (first
                  (flatten
                   (list
                    (if title title
                        (subseq
                         (caddr (car (get-in-toplevel content "title")))
                         6))))))))

  (let ((restructured (flatten (xmls-to-rst (get-content content)
                                            (read-title-index (title page))
                                            page))))
    (setf (toctreep page) (every (lambda (X) (or (includep X)
                                                 (= (length X) 0)))
                                 (mapcar #'strip restructured)))
    (setf (rst page) (format NIL "~{~a~}~%" restructured)))

  '(when (< (length (read-title-index (title page))) 2)
    (format t "~a~%" (title page))))


(defun get-in-toplevel (content tag)
    (loop for entry in content
     if (and (listp entry)
            (string= (car entry) tag))
       collect entry))


(defun get-text (tag)
  (if (listp tag)
      (let ((elements (cddr tag)))
        (loop for element in elements
          collect (get-text element)))
      tag))


(defun flatten (l)
  (cond
    ((null l) NIL)
    ((atom l) (list l))
    ((listp l) (mapcan #'flatten l))))


(defun strip (text)
  (string-trim '(#\Space #\Tab #\Newline) text))

(defun extract-text (content)
  (loop for text in
       (mapcar #'strip
               (flatten (mapcar #'get-text content)))
     if (and text
             (> (length text) 0))
     collect text))


(defun remove-index (text)
  (coerce (let ((done NIL)
          (index-set (coerce "0123456789." 'list)))
            (loop for character in (coerce text 'list)
               if (not (find character index-set))
               do (setq done T)
               if done
               collect character)) 'string))



(defun mktitle (text)
  (strip (remove-index (strip text))))


(defun read-title-index (title)
  (let ((index-set (coerce "0123456789." 'list))
        (numbers '())
        (current-number 0))
    (loop for character in (coerce (strip title) 'list)
       until (not (find character index-set))
       do (if (equal character #\.)
              (progn (push current-number numbers)
                     (setf current-number 0))
              (setf current-number
                      (+ (* current-number 10)
                         (digit-char-p character)))))
    (when (> current-number 0)
      (push current-number numbers))

    (reverse numbers)))


(defun includep (text)
  (and (> (length text) 12)
       (string= (subseq (strip text) 0 12)
                ".. include::")))


(defun fname-from-index (index)
  (when (= (length index) 0)
    (format t "===> ~a~%" index)
    (assert NIL))
  (let ((fname (coerce (format NIL "~2,'0d_" (first index)) 'list)))
    (loop for num in (subseq index 1)
       do (setq fname (append fname (list
                                     (code-char (+ (- num 1)
                                                   (char-code #\a)))))))
    (coerce fname 'string)))


(defun xmls-to-rst (xml pindex page &optional (list-depth -1))
  (cond
    ((stringp xml) (regex-replace-all "`" xml "\\\\`"))
    ((atom xml) NIL)
    ((stringp (first xml))
     (cond
       ((or (string= (first xml) "p")
            (string= (first xml) "dl")
            (string= (first xml) "dt"))
        (format NIL "~{~a~}~%"
                (mapcar (LAMBDA (X) (xmls-to-rst X pindex page list-depth))
                        (cddr xml))))

       ((string= (first xml) "dd")
        (format NIL "    ~{~a~}~%"
                (mapcar (LAMBDA (X) (xmls-to-rst X pindex page list-depth))
                        (cddr xml))))

       ((string= (first xml) "pre")
        (format NIL "~%.. code-block:: common-lisp~%~%~{    ~a~%~}"
                (split #\Newline
                       (format NIL "~{~a~}" (xmls-to-rst (cddr xml) pindex
                                                         page list-depth)))))


       ((string= (first xml) "b")
        (let ((text (format NIL "~{~a~}" (mapcar (LAMBDA (X)
                                                   (xmls-to-rst X pindex page
                                                                list-depth))
                                                 (cddr xml)))))
          (when (not (string= text ""))
            (if (char= (char text 0) #\`) text
                (format NIL "**~a**" text)))))

       ((string= (first xml) "i")
        (let ((text (format NIL "~{~a~}" (mapcar (LAMBDA (X)
                                                   (xmls-to-rst X pindex page
                                                                list-depth))
                                                 (cddr xml)))))
          (when (not (string= text ""))
            (if (char= (char text 0) #\`) text
                (format NIL "*~a*" text)))))

       ((string= (first xml) "ul")
        (format NIL "~{~a~}" (mapcar (LAMBDA (X)
                                       (xmls-to-rst X pindex page
                                                    (1+ list-depth)))
                                       (cddr xml))))

       ((string= (first xml) "h2")
        (let* ((text (format NIL "~{~a~}" (flatten (get-text xml))))
               (title (mktitle text))
               (index (read-title-index text)))

          (if (and index
                   (not (equal index pindex))
                   (> (length index) (length pindex))
                   (equalp (subseq index 0 (length pindex))
                           pindex))
              (format NIL "~%~%.. include:: ~a.rst" (fname-from-index index))
              (progn
                (format NIL "~%~a~%~a~%~%"
                        title
                        (make-string (length title)
                                     :initial-element
                                     (char *UNDERLINES* (length index))))))))

       ((string= (first xml) "a")
        (let ((text
               (string-trim
                '(#\* #\Space #\Tab #\Newline)
                (format NIL "~{~a~}"
                        (flatten
                         (mapcar (LAMBDA (X) (xmls-to-rst X pindex page
                                                          list-depth))
                                 (cddr xml)))))))
          (if (= (length text) 0) ""
              (if (includep text)
                  text
                  (progn
                    (let ((link-text NIL)
                          (props (get-in-toplevel (second xml) "href")))
                      (loop for (key href) in props
                         if (string= key "href") do
                           ;;; Sphinx uses .html files, not .htm
                           (when (not
                                  (some #'(LAMBDA (C) (char= C #\:)) href))
                             (setf href (regex-replace "\.htm" href ".html")))

                           (setf (gethash text (link-table page)) href)
                           (setf link-text (format NIL "`~a`_"  text)))
                      (if link-text link-text
                          text)))))))

       ((string= (first xml) "li")
        (format NIL "~%~a* ~{~a ~}" (make-string (max 0 list-depth)
                                                   :initial-element #\Space)
                (flatten (mapcar
                          (LAMBDA (X) (xmls-to-rst X pindex page list-depth))
                          (cdr xml)))))
       ((listp xml) (mapcar (LAMBDA (X) (xmls-to-rst X pindex
                                                     page list-depth))
                            xml))))
    ((listp xml) (mapcar (LAMBDA (X) (xmls-to-rst X pindex page list-depth))
                         xml))))


(defun get-content (page)
  (let ((hr-count 0))
    (loop for tag in page
       if (and (listp tag)
               (stringp (first tag))
               (string= (first tag) "hr"))
       do (incf hr-count)
       if (and (= hr-count 1)
               (not (and (listp tag)
                         (stringp (first tag))
                         (string= (first tag) "hr"))))
       collect tag)))


(defun parse (fname)
  (with-open-file (f fname)
    (let ((text (make-string (file-length f))))
      (read-sequence text f)
      (node-to-xmls (parse-html5-fragment text) NIL))))


(defun parse-page (&optional (fname "Front/Contents.htm"))
  (let ((front-content (parse fname)))
    (make-instance 'spec-page
                   :content front-content)))

(defun read-docs (&optional (path "./"))
  (loop for f in (list-directory (merge-pathnames path "Body"))
     collect (let ((fname (file-namestring f))
                   (page (parse-page f)))
               (list page fname))))


(defun toctree (spec docs)
  (let ((fstr (make-array '(0) :element-type 'base-char
                          :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s fstr)
      ;; Index name
      (let* ((title (mktitle (title spec)))
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


(defun reload-docs (docs)
  (loop for (spec fname) in docs
     collect (list (make-instance 'spec-page :content (raw spec))
                   fname)))
