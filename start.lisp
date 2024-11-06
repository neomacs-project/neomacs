(in-package #:neomacs)

(defun start (&optional (use-neomacs-debugger t))
  (ceramic:setup)
  (ceramic:start)
  (setf *current-frame-root* (make-frame-root (make-scratch))
        *use-neomacs-debugger* use-neomacs-debugger)
  (start-command-loop))
