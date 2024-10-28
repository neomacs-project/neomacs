(in-package #:neomacs)

(define-mode html-doc-mode (file-mode) ())

(defmethod selectable-p-aux ((buffer html-doc-mode) pos)
  (not (and (member (node-after pos) '(#\Space #\Newline #\Tab))
            (member (node-before pos) '(nil #\Space #\Newline #\Tab)))))

(defmethod revert-buffer-aux ((buffer html-doc-mode))
  (erase-buffer)
  (evaluate-javascript
   (ps:ps (ps:chain (js-buffer buffer) web-contents
                    (load-u-r-l
                     (ps:lisp (str:concat "file://" (uiop:native-namestring (file-path buffer)))))))
   nil))

;;; Get DOM from renderer
;; Initially adapted from Nyxt

(defparameter +get-body-json-code+
  (ps:ps
    (defparameter neomacs-identifier-counter 0)
    (defun process-element (element)
      (let ((object (ps:create :name (ps:@ element node-name)))
            (attributes (ps:chain element attributes)))
        (when (= 1 (ps:@ element node-type))
          (ps:chain element (set-attribute
                             "neomacs-identifier"
                             (ps:stringify neomacs-identifier-counter)))
          (incf neomacs-identifier-counter))
        (unless (ps:undefined attributes)
          (setf (ps:@ object :attributes) (ps:create))
          (loop for i from 0 below (ps:@ attributes length)
                do (setf (ps:@ object :attributes (ps:chain attributes (item i) name))
                         (ps:chain attributes (item i) value))))
        (unless (or (ps:undefined (ps:chain element child-nodes))
                    (= 0 (ps:chain element child-nodes length)))
          (setf (ps:chain object :children)
                (loop for child in (ps:chain element child-nodes)
                      collect (process-element child))))
        (when (and (ps:@ element shadow-root)
                   (ps:@ element shadow-root first-child))
          (setf (ps:chain object :children)
                (loop for child in (ps:chain *array
                                             (from (ps:@ element shadow-root children))
                                             (concat (ps:chain *array (from (ps:@ element children)))))
                      collect (process-element child))))
        (when (or (equal (ps:@ element node-name) "#text")
                  (equal (ps:@ element node-name) "#comment")
                  (equal (ps:@ element node-name) "#cdata-section"))
          (setf (ps:@ object :text) (ps:@ element text-content)))
        object))
    (process-element (ps:@ document body))))

(defun named-json-parse (json)
  "Return a DOM-tree produced from JSON.

JSON should have the format like what `+get-body-json-code+' produces:
- A nested hierarchy of objects (with only one root object), where
  - Every object has a 'name' (usually a tag name or '#text'/'#comment').
  - Some objects can have 'attributes' (a string->string dictionary).
  - Some objects have a subarray ('children') of objects working by these three
    rules."
  (labels ((json-to-dom (json)
             (let ((node
                     (cond
                       ((equal (assoc-value json :name) "#text")
                        (make-instance 'text-node :text (assoc-value json :text)))
                       (t
                        (make-instance 'element :tag-name (str:downcase (assoc-value json :name)))))))
               (dolist (c (assoc-value json :children))
                 (append-child node (json-to-dom c)))
               (iter (for (k . v) in (assoc-value json :attributes))
                 (setf (attribute node (str:downcase (symbol-name k))) v))
               node)))
    (json-to-dom json)))

(defun update-document-model (buffer)
  (let ((dom (named-json-parse
              (evaluate-javascript-sync +get-body-json-code+ buffer))))
    (do-dom (lambda (n) (setf (host n) buffer)) dom)
    (setf (document-root buffer) dom
          (restriction buffer) dom
          (pos (focus buffer)) (pos-down dom))))
