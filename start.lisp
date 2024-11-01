(in-package #:neomacs)

(defun start ()
  (ceramic:start)
  (let* ((buffer (get-buffer-create "*scratch*" :modes '(lisp-mode file-mode)))
         (frame (make-frame-root buffer)))
    (setf *current-frame-root* frame)
    (with-current-buffer buffer
      (setf (file-path buffer)
            (asdf:system-relative-pathname :neomacs #p"scratch.lisp"))
      (revert-buffer)
      (disable 'file-mode))
    (start-command-loop)))
