(in-package #:neomacs)

(defun start ()
  (ceramic:start)
  (let* ((buffer (make-instance 'buffer :name "*scratch*"))
         (buffer-2 (make-instance 'buffer :name "*scratch*"))
         (frame (make-frame-root buffer)))
    (setf *current-frame-root* frame)
    (with-current-buffer buffer
      (enable 'lisp-mode)
      (let ((doc-node (make-element "div" :class "doc")))
        (insert-nodes (end-pos (document-root buffer)) doc-node)
        (setf (restriction buffer) doc-node
              (pos (focus buffer)) (end-pos doc-node))
        (apply #'insert-nodes (end-pos doc-node)
               (read-from-file
                (asdf:system-relative-pathname :neomacs #p"scratch.lisp")))))
    (with-current-buffer buffer-2
      (enable 'lisp-mode)
      (let ((doc-node (make-element "div" :class "doc")))
        (insert-nodes (end-pos (document-root buffer-2)) doc-node)
        (apply #'insert-nodes (end-pos doc-node)
               (read-from-file
                (asdf:system-relative-pathname :neomacs #p"scratch.lisp")))))
    (start-command-loop)))
