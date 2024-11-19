(in-package #:neomacs)

(define-mode inspector-mode (read-only-mode lisp-mode)
  ((for-object
    :initform (alex:required-argument :object)
    :initarg :object)))

(defmethod revert-buffer-aux ((buffer inspector-mode))
  (let ((object (for-object buffer)) *print-pretty*)
    (multiple-value-bind (text label parts) (sb-impl::inspected-parts object)
      (unless label
        (setq parts (mapcar #'cons (alex:iota (length parts)) parts)))
      (insert-nodes
       (end-pos (document-root buffer))
       (dom `(:div ,text))
       (dom
        `(:table
          (:tbody
           ,@ (iter (for (l . v) in parts)
                (collect `(:tr (:td :class "component-name"
                                    ,(princ-to-string l))
                               (:td ,(print-dom v))))))))))))

(define-command inspect-object
    :interactive (lambda () (list (presentation-at (focus))))
  (object)
  (pop-to-buffer
   (make-buffer
    "*inspector*" :mode 'inspector-mode
    :object object :revert t)))

(defsheet inspector-mode
    `(("table" :width "100%"
               :border-collapse "collapse")
      ("td" :padding-right "1em"
            :white-space "nowrap"
            :vertical-align "top")))
