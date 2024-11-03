(in-package #:neomacs)

(defun start ()
  (ceramic:setup)
  (ceramic:start)
  (setf *current-frame-root* (make-frame-root (make-scratch)))
  (start-command-loop))
