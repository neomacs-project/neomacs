;; This buffer is for text that is not saved, and for Lisp evaluation.
;; test

;;; Heading
;;;; subtext

(defun random-tree (depth)
  "Generate a random tree to climb!"
  (case (random 3)
    ((0) 'x)
    ((1) 'y)
    (t (list (random-tree (1- depth))
             (random-tree (1- depth))))))
(default foo bar)
(build-manual)
(defun foo ()
  "test"
  (progn
    (y)
    (print 1)
    x))
(read-yes-or-no "test")