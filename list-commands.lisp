(in-package #:neomacs)

(define-class list-mode () ())

(defgeneric revert-buffer (buffer))
(defgeneric generate-rows (buffer))

(defmethod enable-aux ((mode (eql 'list-mode)))
  (pushnew 'buffer-list (styles (current-buffer))))

(defun make-list-buffer (name mode-symbol)
  (lret ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (enable mode-symbol)
      (revert-buffer buffer))))

(defmethod revert-buffer ((buffer list-mode))
  (erase-buffer buffer)
  (let ((body-node (make-element "tbody")))
    (insert-nodes (end-pos (document-root buffer))
                  (make-element "table" :children (list body-node)))
    (setf (pos (focus)) (end-pos body-node))
    (generate-rows buffer)))

(define-class command-list-mode (list-mode) ())

(defmethod generate-rows ((buffer command-list-mode))
  (iter (for c in *commands*)
    (for name = (string-downcase (symbol-name c)))
    (for doc = (documentation c 'function))
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
  (switch-to-buffer
   (make-list-buffer "*commands*" 'command-list-mode)))

(define-command list-buffers ()
  (switch-to-buffer
   (make-list-buffer "*buffers*" 'buffer-list-mode)))

(defun row-at-focus ()
  (pos-up-ensure (focus) (alex:rcurry #'class-p "row")))

(defun buffer-at-focus ()
  (or (when-let (row (row-at-focus))
        (gethash (parse-integer (attribute row "buffer")) *buffer-table*))
      (error "No focused buffer")))

(defstyle buffer-list `(("table" :white-space "pre")))
