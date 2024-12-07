(in-package #:neomacs)

(sera:export-always
    '(inspect-object))

(define-mode describe-mode (read-only-mode) ())

(define-keys describe-mode
  "q" 'quit-buffer)

;;; Describe key

(define-mode describe-key-mode (describe-mode)
  ((for-key :initform (alex:required-argument :key)
            :initarg :key)
   (for-buffer :initform (alex:required-argument :buffer)
               :initarg :buffer)))

(define-command describe-key
  :interactive
  (lambda ()
    (list (read-key-sequence "Describe the following key: ")))
  (keyseq)
  "Describe function bound to KEYSEQ in current buffer."
  (when keyseq
    (pop-to-buffer
     (make-buffer
      "*describe-key*" :mode 'describe-key-mode
      :key keyseq
      :buffer (current-buffer)
      :revert t))))

(defun lookup-keybind-trace (key buffer)
  (let (cmd trace)
    (iter (for keymap in (reverse (keymaps buffer)))
      (unless keymap (next-iteration))
      (multiple-value-bind (next bind-type)
          (keymap-find-keybind keymap key cmd)
        (when bind-type
          (push (list next (keymap-name keymap) bind-type) trace))
        (setq cmd next)))
    (values cmd trace)))

(defun render-describe-documentation (doc)
  (cons (make-element "h1" :children (list "Documentation:"))
        (if doc
            (let ((paragraphs (str:split "

"
                                         doc)))
              (iter (for p in paragraphs)
                (collecting
                  (make-element "p" :children (render-doc-string-paragraph p)))))
            (list (make-element "p" :children (list "No docstring avaliable."))))))

(defun render-describe-definitions (symbol defs)
  (let ((*print-case* :downcase))
    (list (make-element "h1" :children (list "Definitions:"))
          (dom `(:table
                 (:tbody
                  ,@(iter
                      (for (type def) in defs)
                      (collect (render-xref-definition symbol type def)))))))))

(defmethod revert-buffer-aux ((buffer describe-key-mode))
  (erase-buffer)
  (multiple-value-bind (cmd trace)
      (lookup-keybind-trace (for-key buffer)
                            (for-buffer buffer))
    (if cmd
        (progn
          (insert-nodes
           (end-pos (document-root buffer))
           (dom `(:div
                  (:code ,(key-description (for-key buffer)))
                  " is bound to "
                  ,(print-dom cmd)
                  " in "
                  ,(print-dom (for-buffer buffer)))))
          (insert-nodes
           (end-pos (document-root buffer))
           (make-element "h1" :children (list "Bindings:"))
           (dom
            (cons :div (iter (for (cmd name bind-type) in trace)
                         (ecase bind-type
                           ((:key)
                            (collecting
                              `(:p "bound to " ,(print-dom cmd)
                                   " by " ,(print-dom name))))
                           ((:function)
                            (collecting
                              `(:p "translated to " ,(print-dom cmd)
                                   " by " ,(print-dom name)))))))))
          (apply #'insert-nodes
                 (end-pos (document-root buffer))
                 (render-describe-definitions
                  cmd
                  (find-definitions
                   cmd '(:function :generic-function :method))))
          (apply #'insert-nodes
                 (end-pos (document-root buffer))
                 (render-describe-documentation (documentation cmd 'function))))
        (progn
          (insert-nodes
           (end-pos (document-root buffer))
           (dom `(:div
                  (:code ,(key-description (for-key buffer)))
                  " is unbound in "
                  ,(print-dom (for-buffer buffer)))))))))

;;; Describe bindings

(define-mode keymap-list-mode (list-mode) ())

(defmethod generate-rows ((buffer keymap-list-mode))
  (let ((*print-case* :downcase))
    (iter (for (name keymap) in-hashtable *keymap-table*)
      (collect (attach-presentation
                (dom `(:tr (:td ,(princ-to-string name))))
                keymap)))))

(defun render-describe-keymap-rows (keymap)
  (let (rows)
     (traverse-keymap
      keymap
      (lambda (kseq cmd)
        (push (dom `(:tr (:td ,(key-description kseq))
                         (:td ,(print-dom cmd))))
              rows)))
     (nreverse rows)))

(define-mode describe-keymap-mode (describe-mode list-mode)
  ((for-keymap :initform (alex:required-argument :keymap)
               :initarg :keymap)))

(define-command describe-keymap
  :interactive
  (lambda ()
    (list (completing-read "Describe keymap: " 'keymap-list-mode)))
  (keymap)
  "Describe key bindings in KEYMAP."
  (pop-to-buffer
   (make-buffer
    "*describe-keymap*" :mode 'describe-keymap-mode
                        :keymap keymap :revert t)))

(defmethod generate-rows ((buffer describe-keymap-mode))
  (render-describe-keymap-rows (for-keymap buffer)))

(defmethod revert-buffer-aux ((buffer describe-keymap-mode))
  (let ((*package* (find-package "NEOMACS-USER")))
    (call-next-method)
    (insert-nodes
     (pos-down (document-root buffer))
     (make-element
      "div" :class "header"
      :children (list (print-dom (keymap-name (for-keymap buffer))))))))

(define-mode describe-bindings-mode (describe-mode list-mode)
  ((for-buffer :initform (alex:required-argument :buffer)
               :initarg :buffer)))

(define-command describe-bindings ()
  "Describe key bindings in currently active keymaps."
  (pop-to-buffer
   (make-buffer
    "*describe-bindings*" :mode 'describe-bindings-mode
    :buffer (current-buffer) :revert t)))

(defmethod revert-buffer-aux ((buffer describe-bindings-mode))
  (erase-buffer)
  (let ((*package* (find-package "NEOMACS-USER")))
    (iter (for keymap in (keymaps (for-buffer buffer)))
      (insert-nodes
       (end-pos (document-root buffer))
       (dom `(:div :class "header"
                   ,(print-dom (keymap-name keymap))))
       (dom `(:table
              (:tbody ,@ (render-describe-keymap-rows keymap)))))))
  (setf (pos (focus buffer))
        (pos-down (document-root buffer))))

;;; Inspector

(define-mode inspector-mode (describe-mode lisp-mode)
  ((for-object
    :initform (alex:required-argument :object)
    :initarg :object)))

(defgeneric render-inspect-object (object)
  (:method ((object t)))
  (:documentation "Render extra information for OBJECT in the inspector."))

(defmethod revert-buffer-aux ((buffer inspector-mode))
  (erase-buffer)
  (let ((object (for-object buffer)) *print-pretty*)
    (multiple-value-bind (text label parts) (sb-impl::inspected-parts object)
      (unless label
        (setq parts (mapcar #'cons (alex:iota (length parts)) parts)))
      (insert-nodes (end-pos (document-root buffer))
                    (make-element "p" :children (list text)))
      (apply #'insert-nodes (end-pos (document-root buffer))
             (render-inspect-object object))
      (insert-nodes
       (end-pos (document-root buffer))
       (make-element "h1" :children (list "Slots:"))
       (dom `(:table
              (:tbody
               ,@ (iter (for (l . v) in parts)
                    (collect `(:tr (:td :class "component-name"
                                        ,(princ-to-string l))
                                   (:td ,(print-dom v))))))))))
    (setf (pos (focus buffer))
          (or (block nil
                (do-elements
                    (lambda (node)
                      (when (tag-name-p node "tr")
                        (return node)))
                  (document-root buffer)))
              (pos-down (document-root buffer))))))

(defmethod render-inspect-object ((object standard-generic-function))
  (append
   (when-let (name (sb-mop:generic-function-name object))
     (render-describe-definitions
      name (find-definitions name '(:generic-function :method))))
   (render-describe-documentation (documentation object t))))

(defmethod render-inspect-object ((object function))
  (append
   (match (swank/backend:function-name object)
     ((list 'macro-function name)
      (render-describe-definitions
       name (find-definitions name '(:macro))))
     ((list 'setf name)
      (render-describe-definitions
       name (find-definitions (list 'setf name) '(:function))))
     ((and name (type symbol))
      (render-describe-definitions
       name (find-definitions name '(:function)))))
   (render-describe-documentation (documentation object t))))

(defun inspect-object (object)
  (pop-to-buffer (make-buffer
                  "*inspector*" :mode 'inspector-mode
                  :object object :revert t)))

(define-command eval-expression-inspect ()
  "Evaluate a Lisp expression from minibuffer and inspect the result."
  (let ((*package* (current-package)))
    (inspect-object
     (eval (read-from-minibuffer
            (format nil "Eval (~a): " (package-name *package*))
            :mode 'lisp-minibuffer-mode)))))

(define-command inspect-presentation ()
  "Inspect object presentation under focus."
  (inspect-object (presentation-at (focus))))

;;; Styles

(defsheet describe-mode
    `(("p" :margin-top 0 :margin-bottom 0)
      ("h1" :margin-top 0 :margin-bottom 0
            :font-size "1.2rem")))

(defsheet inspector-mode
    `(("table" :width "100vw"
               :border-collapse "collapse")
      ("td" :padding-right "1em"
            :white-space "nowrap"
            :vertical-align "top")))
