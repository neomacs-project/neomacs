(in-package #:neomacs)

(defun start (&optional (use-neomacs-debugger t))
  (load-web-history)
  (unless ceramic.runtime:*releasep*
    (ceramic:setup))
  (ceramic:start)
  (setf *current-frame-root* (make-frame-root (make-scratch))
        *use-neomacs-debugger* use-neomacs-debugger)
  (start-command-loop))

(in-package #:ceramic-entry)

(defun neomacs ()
  (let ((ceramic.runtime:*releasep* t))
    (neomacs::start)
    (sb-thread:join-thread neomacs::*command-loop-thread*)))
