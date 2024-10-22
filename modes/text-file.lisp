(in-package #:neomacs)

(define-class text-file-mode (file-mode) ())

(defmethod revert-buffer-aux ((buffer text-file-mode))
  (erase-buffer)
  (let ((doc-node (make-element "div" :class "doc")))
    (insert-nodes (end-pos (document-root buffer)) doc-node)
    (with-open-file (s (file-path buffer) :direction :input)
      (iter (for (values line missing-new-line-p) = (read-line s nil nil))
        (when (> (length line) 0)
          (insert-nodes (end-pos doc-node)
                        (make-instance 'text-node :text line)))
        (if (or missing-new-line-p (not line))
            (return)
            (insert-nodes (end-pos doc-node) (make-new-line-node)))))
    (setf (restriction buffer) doc-node
          (pos (focus buffer)) (pos-down doc-node))))

(defmethod write-file ((buffer text-file-mode))
  (with-open-file (s (file-path buffer)
                     :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (dolist (c (child-nodes
                  (only-elt (get-elements-by-class-name
                             (document-root buffer)
                             "doc"))))
        (cond ((text-node-p c)
               (write-string (text c) s))
              ((new-line-node-p c)
               (terpri s))
              (t (warn "Unrecognized DOM node: ~a" c)))))))
