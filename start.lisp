(in-package #:neomacs)

(defun start (&optional (use-neomacs-debugger t))
  (let ((config-file (uiop:xdg-config-home "neomacs" "init.lisp")))
    (if (uiop:file-exists-p config-file)
        (progn
          (format t "Loading ~a.~%" config-file)
          (load config-file))
        (format t "~a not yet exist.~%" config-file)))
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
