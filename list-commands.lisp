(in-package #:neomacs)

(define-mode list-mode ()
  ((body-node :initform (make-element "tbody"))))

(defgeneric update-list-buffer (mode)
  (:method ((mode list-mode)))
  (:method :before ((mode list-mode))
    (delete-nodes (pos-down (body-node mode)) nil)))

(defmethod enable ((mode list-mode))
  (pushnew 'buffer-list (styles (current-buffer)))
  (insert-nodes (end-pos (document-root (current-buffer)))
                (make-element "table" :children
                              (list (body-node mode)))))

(defun make-list-buffer (name mode-symbol)
  (lret ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (ensure mode-symbol)
      (update-list-buffer (find-submode mode-symbol)))))

(define-mode command-list-mode (list-mode) ())

(defmethod update-list-buffer ((mode command-list-mode))
  (iter (for c in *commands*)
    (for name = (string-downcase (symbol-name c)))
    (for doc = (documentation c 'function))
    (insert-nodes (end-pos (body-node mode))
                  (make-element
                   "tr" :class "row" :children
                   (list (make-element "td" :children (list name))
                         (make-element "td" :children (when doc (list doc))))))))

(define-mode buffer-list-mode (list-mode)
  ((keymap
    :default
    (lret ((m (make-keymap :name "buffer-list")))
      (define-keys m
        "enter" 'switch-to-buffer
        "k" 'delete-buffer
        "d" 'delete-buffer)))))

(defmethod update-list-buffer ((mode buffer-list-mode))
  (iter (for (name buffer) in-hashtable *buffer-name-table*)
    (for modes =
         (when (modes buffer)
           (sera:mapconcat (lambda (m)
                             (string-downcase (symbol-name (mode-name m))))
                           (modes buffer) " ")))
    (insert-nodes (end-pos (body-node mode))
                  (make-element
                   "tr" :class "row" :buffer (id buffer) :children
                   (list (make-element "td" :children (list name))
                         (make-element "td" :children (when modes
                                                        (list modes))))))))

(define-command list-commands ()
  (switch-to-buffer
   (make-list-buffer "*commands*" 'command-list-mode)))

(define-command list-buffers ()
  (switch-to-buffer
   (make-list-buffer "*buffers*" 'buffer-list-mode)))

(defun focused-row ()
  (pos-up-ensure (focus) (alex:rcurry #'class-p "row")))

(defun focused-buffer ()
  (or (when-let (row (focused-row))
        (gethash (parse-integer (attribute row "buffer")) *buffer-table*))
      (error "No focused buffer")))

(defstyle buffer-list `(("table" :white-space "pre")))
