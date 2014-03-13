(in-package #:bluespec)

(defun flatten (l)
  "Convert a nested list structure into a plain one."
  (cond
    ((null l) NIL)
    ((atom l) (list l))
    ((listp l) (mapcan #'flatten l))))


(defun strip (text)
  "Remove whitespaces from both the beggining and end."
  (string-trim '(#\Space #\Tab #\Newline) text))

(defun read-title-index (title)
  "Parse a title hierarchy."
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


(defun remove-index (title)
  "Remove the numerical hierarchy from a title."
  (let ((text (strip title)))
    (strip (coerce (let ((done NIL)
                         (index-set (coerce "0123456789." 'list)))
                     (loop for character in (coerce text 'list)
                        if (not (find character index-set))
                        do (setq done T)
                        if done
                        collect character)) 'string))))
