(in-package #:neomacs)

(defun plump:make-element (parent tag)
  (append-child parent (make-instance 'element :tag-name tag)))

(defun plump:make-text-node (parent &optional (text ""))
  (append-child parent (make-instance 'text-node :text text)))

(defun plump:make-root () (make-instance 'element :tag-name "body"))

(defun plump:attribute (element attribute)
  (attribute element attribute))

(defun (setf plump:attribute) (new-val element attribute)
  (setf (attribute element attribute) new-val))

(defun dom (sexp)
  (first-child (plump-sexp:parse sexp)))
