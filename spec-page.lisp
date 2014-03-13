(in-package #:bluespec)

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
    (setf (rst page) (format NIL "~{~a~}~%" restructured))))


(defun reload-docs (docs)
  (loop for (spec fname) in docs
     collect (list (make-instance 'spec-page :content (raw spec))
                   fname)))
