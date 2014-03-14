(in-package #:bluespec)

(defun get-in-toplevel (content tag)
  "Retrieve all string entries of the list."
  (loop for entry in content
     if (and (listp entry)
             (string= (car entry) tag))
     collect entry))


(defun get-content (page)
  "Extract the data containing the page contents."
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


(defun get-text (tag)
  "Retrieve the text nodes inside the tag."
  (if (listp tag)
      (let ((elements (cddr tag)))
        (loop for element in elements
           collect (get-text element)))
      tag))


(defun includep (text)
  "Check whether the line contains a inclusion directive."
  (and (> (length text) 12)
       (string= (subseq (strip text) 0 12)
                ".. include::")))


(defun fname-from-index (index)
  "Guess the assigned file name given the hierarchical index."
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
  "Convert a XML tree into it's RST representation."
  (cond
    ((stringp xml) (regex-replace-all (coerce '(#\Newline #\Space #\+) 'string)
                                      (regex-replace-all "`" xml "\\\\`")
                                      (coerce '(#\Newline) 'string)))
    ((atom xml) NIL)
    ((stringp (first xml))
     (cond
       ((or (string= (first xml) "p")
            (string= (first xml) "dl")
            (string= (first xml) "dt"))
        (format NIL "~{~a~}~%~%"
                (mapcar (LAMBDA (X) (xmls-to-rst X pindex page list-depth))
                        (cddr xml))))

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
               (title (remove-index text))
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
