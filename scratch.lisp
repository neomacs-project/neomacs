;; This buffer is for text that is not saved, and for Lisp evaluation.

;;; Heading
;;;; subtext

(defun random-tree (depth)
  "Generate a random tree to climb!"
  (case (random 3)
    ((0) 'x)
    ((1) 'y)
    (t (list (random-tree (1- depth))
             (random-tree (1- depth))))))
