(in-package #:neomacs)

(define-mode html-doc-mode (file-mode) ())

(define-keys html-doc-mode
  "enter" 'open-paragraph
  "M-*" 'open-heading
  "M-`" 'open-code
  "M-/" 'open-italic
  "M--" 'open-list
  "M-@" 'open-at)

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

(defmethod on-buffer-loaded progn ((buffer html-doc-mode))
  (update-document-model buffer)
  (setf (pos (focus buffer))
        (pos-down (document-root buffer))))

(defmethod self-insert-aux
    ((buffer html-doc-mode) marker string)
  (let ((node (node-containing marker)))
    (if (member (tag-name node) '("p" "code" "i" "li" "span")
                :test 'equal)
        (insert-nodes marker string)
        (let ((node (make-element "p" :children (list string))))
          (insert-nodes marker node)
          (setf (pos marker) (end-pos node))))))

(define-command open-paragraph
  :mode html-doc-mode (&optional (marker (focus)))
  (let* ((pos (resolve-marker marker))
         (new-node (make-element "p"))
         (dst (pos-right (pos-up pos))))
    (insert-nodes dst new-node)
    (move-nodes pos nil (end-pos new-node))
    (setf (pos marker) (pos-down new-node))))

(define-command open-heading
  :mode html-doc-mode (&optional (marker (focus)))
  (labels ((cycle-level (n)
             (lret ((n (mod (1+ n) 7)))
               (message "Heading Level -> ~a" n)))
           (tag-level (tag)
             (cond ((equal tag "p") 0)
                   ((ppcre:all-matches "^h[123456]$" tag)
                    (parse-integer (subseq tag 1)))))
           (level-tag (level)
             (if (= level 0) "p"
                 (format nil "h~a" level))))
    (let* ((node (node-containing marker))
           (new-node (make-element
                      (level-tag
                       (cycle-level
                        (tag-level (tag-name node)))))))
      (insert-nodes (pos-right node) new-node)
      (move-nodes (pos-down node) nil (end-pos new-node))
      (delete-node node))))

(define-command open-code
  :mode html-doc-mode (&optional (marker (focus)))
  (let ((node (make-element "code")))
    (insert-nodes marker node)
    (setf (pos marker) (end-pos node))))

(define-command open-italic
  :mode html-doc-mode (&optional (marker (focus)))
  (let ((node (make-element "i")))
    (insert-nodes marker node)
    (setf (pos marker) (end-pos node))))

(define-command open-list
  :mode html-doc-mode (&optional (marker (focus)))
  (unless (tag-name-p (node-containing marker) "ul")
    (let ((node (make-element "ul")))
      (insert-nodes marker node)
      (setf (pos marker) (end-pos node))))
  (let ((node (make-element "li")))
    (insert-nodes marker node)
    (setf (pos marker) (end-pos node))))

(define-command open-at
  :mode html-doc-mode (&optional (marker (focus)))
  (let ((node (make-element "span" :class "at-expr")))
    (insert-nodes marker node)
    (setf (pos marker) (end-pos node))))

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
    (list (process-element (ps:@ document body))
          neomacs-identifier-counter)))

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
  (bind (((json id) (evaluate-javascript-sync +get-body-json-code+ buffer))
         (dom (named-json-parse json)))
    (do-dom (lambda (n) (setf (host n) buffer)) dom)
    (setf (document-root buffer) dom
          (restriction buffer) dom
          (pos (focus buffer)) (pos-down dom)
          (next-neomacs-id buffer) id)))

(defmethod write-dom-aux ((buffer html-doc-mode) node stream)
  (let ((*serialize-exclude-attributes* '("neomacs-identifier")))
    (serialize node stream)))

(defun print-arglist (arglist package)
  (let ((*package* package))
    (format nil "(~{~a~^ ~})" arglist)))

(in-nomine:define-namespace at-expander)

(setf (symbol-at-expander 'fundoc)
      (lambda (arg)
        (let* ((function (read-from-string arg))
               (doc (documentation function 'function)))
          (list* (make-instance 'text-node :text "Function: ")
                (make-element "code" :children (list arg))
                (make-instance 'text-node :text " ")
                (make-element "code" :children
                              (list (print-arglist (swank-backend:arglist function)
                                                   (symbol-package function))))
                (when doc (list (make-element "p" :children
                                              (list doc))))))))

(setf (symbol-at-expander 'classdoc)
      (lambda (arg)
        (let ((class (read-from-string arg)))
          (list (make-instance 'text-node :text "Class: ")
                (make-element "code" :children (list arg))))))

(defun expand-at-expr (node)
  (only-elt
   (map-dom
    (lambda (node results)
      (if (class-p node "at-expr")
          (bind (((name arg) (str:split " "(text-content node))))
            (funcall (symbol-at-expander
                      (find-symbol (string-upcase name)))
                     arg))
          (list
           (lret ((new (clone-node node nil)))
             (iter (for ns in results)
               (append-children new ns))))))
    node)))

(define-command render-html-doc
  :mode html-doc-mode ()
  "Render current buffer by expanding at expressions."
  (let ((path (file-path (current-buffer))))
    (with-open-file (s (make-pathname
                        :name (str:concat (pathname-name path)
                                          "-rendered")
                        :defaults path)
                       :direction :output
                       :if-exists :supersede)
      (let ((*serialize-exclude-attributes* '("neomacs-identifier"))
            (*package* (find-package "NEOMACS")))
        (serialize
         (expand-at-expr (document-root (current-buffer)))
         s)))))

(defstyle html-doc-mode
    `((":empty::after" :content "_")
      (".at-expr::before" :content "@")
      (".at-expr" :border "solid 1px currentColor")))
