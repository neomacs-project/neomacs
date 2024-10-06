(in-package #:neomacs)

(define-class minibuffer-mode () ())

(define-keymap minibuffer-mode ()
  "enter" 'exit-minibuffer
  "C-g" 'abort-minibuffer)

(defmethod selectable-p-aux ((buffer minibuffer-mode) pos)
  (class-p (node-containing pos) "input"))

(define-command exit-minibuffer ()
  (error 'exit-recursive-edit))

(define-command abort-minibuffer ()
  (error 'exit-recursive-edit :condition 'quit))

(defun make-minibuffer ()
  (lret ((buffer (make-instance 'buffer :name " *minibuffer*" :styles '(buffer minibuffer))))
    (setf (window-decoration buffer)
          (make-element
           "div" :class "minibuffer" :selectable ""
           :children (list (make-element "div" :class "content" :buffer (id buffer)))))))

(defun read-from-minibuffer (prompt)
  (with-current-buffer (current-frame-root)
    (let* ((minibuf (make-minibuffer))
           (pos (npos-prev-ensure (end-pos (document-root (current-buffer)))
                                  (alex:compose (alex:rcurry #'class-p "buffer") #'node-before)))
           (window-node (window-decoration minibuf)))
      (insert-nodes pos window-node)
      (redisplay-windows)
      (setf (pos (focus)) window-node)
      (unwind-protect
           (with-current-buffer minibuf
             (let ((input (make-element "span" :class "input")))
               (insert-nodes (end-pos (document-root (current-buffer)))
                             (make-element "span" :children (list prompt))
                             input)
               (setf (pos (focus)) (end-pos input))
               (enable 'minibuffer-mode)
               (recursive-edit)
               (text (first-child input))))
        (delete-nodes window-node (pos-right window-node))
        (delete-buffer minibuf)))))

(define-command execute-command ()
  (let ((name (read-from-minibuffer "M-x ")))
    (if-let (cmd (find name *commands*
                       :key (alex:compose #'string-downcase #'symbol-name)
                       :test 'equal))
      (funcall cmd)
      (echo "No such command"))))

(define-key *global-keymap*
  "M-x" 'execute-command)
