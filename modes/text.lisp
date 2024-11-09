(in-package #:neomacs)

(define-mode text-mode (doc-mode) ())

(defmethod read-dom-aux ((buffer text-mode) stream)
  (bind (((:values line missing-new-line-p) (read-line stream)))
    (when (> (length line) 0)
      (append-text *dom-output* line))
    (unless (or missing-new-line-p (not line))
      (append-child *dom-output* (make-new-line-node)))))

(defmethod write-dom-aux ((buffer text-mode) node stream)
  (cond ((text-node-p node)
         (write-string (text node) stream))
        ((new-line-node-p node)
         (terpri stream))
        (t (warn "Unrecognized DOM node: ~a" node))))
