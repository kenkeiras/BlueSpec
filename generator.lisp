(ql:quickload "cl-html5-parser")
(ql:quickload "cl-fad")

(use-package :html5-parser)
(use-package :cl-fad)


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


(defun xmls-to-rst (xml &optional (tchar #\-))
  (cond
    ((stringp xml) xml)
    ((atom xml) NIL)
    ((stringp (first xml))
     (cond
       ((string= (first xml) "p")
        (append (mapcar #'xmls-to-rst (cddr xml))
                '(#\Newline)))

       ((string= (first xml) "h2")
        (let ((title (mktitle (format NIL "~{~a~}" (flatten (get-text xml))))))
          (format NIL "~%~a~%~a~%~%"
                  title
                  (make-string (length title)  :initial-element tchar))))

       ((string= (first xml) "a")
        (let ((text (strip (format NIL "~{~a~}" (flatten (get-text xml))))))
          (when (> (length text) 0)
            (format NIL "`~{~a~}`_"  (flatten (get-text xml))))))

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
          (strip (format t "~{~a~^ ~}"
                         (flatten
                          (list
                           (if title title
                               (subseq
                                (caddr (car (get-in-toplevel content "title")))
                                6)))))))))


(defun parse (fname)
  (with-open-file (f fname)
    (let ((text (make-string (file-length f))))
      (read-sequence text f)
      (node-to-xmls (parse-html5-fragment text)))))


(defun parse-page (&optional (fname "Front/Contents.htm"))
  (let ((front-content (parse fname)))
    (make-instance 'spec-page
                   :content front-content)))

(defun read-docs ()
  (loop for f in (list-directory "Body")
     collect (let ((fname (file-namestring f))
                   (page (parse-page f)))
               (list page fname))))


(defun print-docs (docs)
    (loop for (spec fname) in docs do
         (let ((oname (format NIL "~a.rst"
                              (subseq fname 0 (- (length fname) 4)))))
           ;; (format t "~a -> ~a~%" fname oname)
           (with-open-file (f oname :direction :output)
             (format f "~a" (rst spec))))))
