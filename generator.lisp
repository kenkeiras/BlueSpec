(ql:quickload "cl-html5-parser")
(ql:quickload "cl-fad")

(use-package :html5-parser)
(use-package :cl-fad)


(defvar *LINK-TABLE* (make-hash-table :test #'equalp))


(defclass spec-page()
  ((title :accessor title)
   (rst :accessor rst)
   (links :accessor links)
   ))


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


(defun xmls-to-rst (xml &optional (tchar #\-))
  (cond
    ((stringp xml) xml)
    ((atom xml) NIL)
    ((stringp (first xml))
     (cond
       ((string= (first xml) "p")
        (append (mapcar #'xmls-to-rst (cddr xml))
                '(#\Newline)))

       ((string= (first xml) "i")
        (format NIL "*~{~a~}*" (mapcar #'xmls-to-rst (cddr xml))))

       ((string= (first xml) "h2")
        (let ((title (mktitle (format NIL "~{~a~}" (flatten (get-text xml))))))
          (format NIL "~%~a~%~a~%~%"
                  title
                  (make-string (length title)  :initial-element tchar))))

       ((string= (first xml) "a")
        (let ((text (strip (format NIL "~{~a~}" (flatten (get-text xml))))))
          (when (> (length text) 0)
            (let ((href (get-in-toplevel (second xml) "href")))
              (when (> (length href) 0)
                (setf (gethash text *LINK-TABLE*) (second (first href)))))
            (format NIL "`~a`_"  text))))

       ((listp xml) (mapcar #'xmls-to-rst xml))))
     ((listp xml) (mapcar #'xmls-to-rst xml))))


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


(defmethod initialize-instance :after ((page spec-page)
                                       &key ((:content content)))
  ;; (setf (fname page) name)
  (setf (rst page) (format NIL "~{~a~}~%" (flatten (xmls-to-rst
                                                  (get-content content)))))

  (let ((title (get-text (car (get-in-toplevel content "h2")))))
    (setf (title page)
          (strip (first
                  (flatten
                   (list
                    (if title title
                        (subseq
                         (caddr (car (get-in-toplevel content "title")))
                         6))))))))

  (when (< (length (read-title-index (title page))) 2)
    (format t "~a~%" (title page))))


(defun parse (fname)
  (with-open-file (f fname)
    (let ((text (make-string (file-length f))))
      (read-sequence text f)
      (node-to-xmls (parse-html5-fragment text)))))


(defun parse-page (&optional (fname "Front/Contents.htm"))
  (let ((front-content (parse fname)))
    (make-instance 'spec-page
                   :content front-content)))

(defun read-docs (&optional (path "."))
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
                      (equalp (subseq page-index (length index)) index))
                 (format s "   ~a~%" (subseq fname 0 (- (length fname) 4)))))))

      fstr)))


(defun print-docs (docs)
  (with-open-file (index "index.rst" :direction :output)
    (format index "Welcome to BlueSpec's documentation!
====================================

Contents:

.. toctree::
   :maxdepth: 2
   :numbered:

")
    (loop for (spec fname) in docs do
         (let ((oname (format NIL "~a.rst"
                              (subseq fname 0 (- (length fname) 4)))))
           ;; (format t "~a -> ~a~%" fname oname)
           (with-open-file (f oname :direction :output)
             (if (= (length (read-title-index (title spec))) 1)
                 (progn (format index "   ~a~%"
                                (subseq fname 0 (- (length fname) 4)))
                        (format f "~a" (strip (toctree spec docs))))
                 (format f "~a" (strip (rst spec)))))))

    (format index "~%~%")
    (loop for key being the hash-keys of *LINK-TABLE*
       using (hash-value value)
         do (format index ".. _~a: ~a~%" key value))))
