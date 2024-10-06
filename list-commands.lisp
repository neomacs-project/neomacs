(in-package #:neomacs)

(define-class list-mode () ())

(defgeneric generate-rows (buffer))

(defmethod enable-aux ((mode (eql 'list-mode)))
  (pushnew 'buffer-list (styles (current-buffer))))

(defmethod revert-buffer-aux ((buffer list-mode))
  (erase-buffer buffer)
  (let ((body-node (make-element "tbody")))
    (insert-nodes (end-pos (document-root buffer))
                  (make-element "table" :children (list body-node)))
    (setf (pos (focus)) (end-pos body-node))
    (generate-rows buffer)))

(define-class command-list-mode (list-mode) ())

(defun short-doc (command)
  (let ((doc (documentation command 'function)))
    (subseq doc 0 (position #\Newline doc))))

(defmethod generate-rows ((buffer command-list-mode))
  (iter (for c in *commands*)
    (for name = (string-downcase (symbol-name c)))
    (for doc = (short-doc c))
    (insert-nodes (focus)
                  (make-element
                   "tr" :class "row" :children
                   (list (make-element "td" :children (list name))
                         (make-element "td" :children (when doc (list doc))))))))

(define-class buffer-list-mode (list-mode) ())

(define-keymap buffer-list-mode ()
  "enter" 'switch-to-buffer
  "k" 'delete-buffer
  "d" 'delete-buffer)

(defmethod generate-rows ((buffer buffer-list-mode))
  (iter (for (name buf) in-hashtable *buffer-name-table*)
    (for modes =
         (sera:mapconcat (alex:compose #'string-downcase #'symbol-name)
                         (mapcar #'class-name (sb-mop:class-precedence-list (class-of buf)))
                         " "))
    (insert-nodes (focus)
                  (make-element
                   "tr" :class "row" :buffer (id buf) :children
                   (list (make-element "td" :children (list name))
                         (make-element "td" :children (when (> (length modes) 0)
                                                        (list modes))))))))

(define-command list-commands ()
  (with-current-buffer
      (switch-to-buffer
       (get-buffer-create "*commands*" :mixins '(command-list-mode)))
    (revert-buffer)))

(define-command list-buffers ()
  (with-current-buffer
      (switch-to-buffer
       (get-buffer-create "*buffers*" :mixins '(buffer-list-mode)))
    (revert-buffer)))

(defun row-at-focus ()
  (pos-up-ensure (focus) (alex:rcurry #'class-p "row")))

(defun buffer-at-focus ()
  (or (when-let (row (row-at-focus))
        (gethash (parse-integer (attribute row "buffer")) *buffer-table*))
      (error "No focused buffer")))

(defstyle buffer-list `(("table" :white-space "pre")))
