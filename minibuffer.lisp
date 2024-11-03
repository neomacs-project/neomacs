(in-package #:neomacs)

(define-mode minibuffer-mode () ((prompt :initarg :prompt)))

(define-keys minibuffer-mode
  "enter" 'exit-minibuffer
  "C-g" 'abort-minibuffer)

(defmethod selectable-p-aux ((buffer minibuffer-mode) pos)
  (class-p (node-containing pos) "input"))

(defmethod check-read-only ((buffer minibuffer-mode) pos)
  (unless (class-p (node-containing pos) "input")
    (error 'element-read-only-error :element (node-containing pos))))

(defmethod window-decoration-aux ((buffer minibuffer-mode))
  (dom `((:div :class "minibuffer" :selectable "")
         ((:div :class "main content" :buffer ,(id buffer))))))

(define-command exit-minibuffer
  :mode minibuffer-mode ()
  (error 'exit-recursive-edit))

(define-command abort-minibuffer
  :mode minibuffer-mode ()
  (error 'exit-recursive-edit :condition 'quit))

(defun minibuffer-input-element (buffer)
  (only-elt (get-elements-by-class-name (document-root buffer) "input")))

(defmethod minibuffer-input ((buffer minibuffer-mode))
  (text-content (minibuffer-input-element buffer)))

(defmethod revert-buffer-aux ((buffer minibuffer-mode))
  (let ((input (dom `((:span :class "input")))))
    (insert-nodes (end-pos (document-root (current-buffer)))
                  (dom `((:span :class "prompt")
                         ,(prompt buffer)))
                  input)
    (setf (pos (focus)) (end-pos input))))

(defun read-from-minibuffer (prompt &rest args)
  "Read and return a string from minibuffer with PROMPT.

ARGS are passed to `make-buffer' to create the minibuffer."
  (unless (getf args :modes)
    (setf (getf args :modes) 'minibuffer-mode))
  (let ((minibuf
          (apply #'make-buffer "*minibuffer*"
                 :prompt prompt args))
        (saved-focus (focused-buffer)))
    (display-buffer-below minibuf)
    (focus-buffer minibuf)
    (unwind-protect
         (with-current-buffer minibuf
           (revert-buffer)
           (recursive-edit)
           (minibuffer-input minibuf))
      (focus-buffer saved-focus)
      (close-buffer-display minibuf)
      (delete-buffer minibuf))))

(defun read-yes-or-no (prompt)
  (let ((answer (read-from-minibuffer
                 (str:concat prompt "(yes or no) "))))
    (cond ((member answer '("y" "yes") :test 'equal)
           t)
          ((member answer '("n" "no") :test 'equal)
           nil)
          (t (read-yes-or-no prompt)))))

(defun completing-read (prompt list-mode &rest args)
  "Read and return a string from minibuffer with completion.

This is a thin wrapper around `read-from-minibuffer' that creates a completion buffer in LIST-MODE."
  (read-from-minibuffer
   prompt
   :modes 'minibuffer-completion-mode
   :completion-buffer
   (apply #'make-completion-buffer
          (list list-mode 'completion-buffer-mode)
          args)))

(define-mode completion-mode ()
  ((completion-buffer :initarg :completion-buffer))
  (:documentation
   "Abstract mode for buffers that has a completion buffer."))

(define-mode minibuffer-completion-mode
    (completion-mode minibuffer-mode) ()
  (:documentation
   "Mode for minibuffer that supports completion."))

(define-keys minibuffer-completion-mode
  "tab" 'complete-minibuffer
  'exit-minibuffer 'complete-exit-minibuffer
  'next-line 'next-completion
  'previous-line 'previous-completion
  'scroll-down-command 'scroll-down-completion
  'scroll-up-command 'scroll-up-completion
  'beginning-of-buffer 'first-completion
  'end-of-buffer 'last-completion)

(defgeneric update-completion-buffer (buffer)
  (:method ((buffer minibuffer-completion-mode))
    (setf (occur-query (completion-buffer buffer))
          (minibuffer-input buffer))))

(defgeneric complete-minibuffer-aux (buffer)
  (:method ((buffer minibuffer-completion-mode))
    (let ((input (only-elt (get-elements-by-class-name
                            (document-root buffer) "input")))
          (selection (node-after (focus (completion-buffer buffer)))))
      (unless (class-p selection "dummy-row")
        (delete-nodes (pos-down input) nil)
        (insert-nodes (pos-down input)
                      (text-content (first-child selection)))))))

(defmethod on-post-command progn ((buffer minibuffer-completion-mode))
  (update-completion-buffer buffer))

(define-mode completion-buffer-mode (occur-mode)
  ((require-match
    :initform t :initarg :require-match
    :documentation
    "Whether a match is required.

If this slot is NIL, an invisible selectable dummy row is inserted at
the beginning of the completion buffer. No completion is performed
when this row is selected.")))

(defmethod generate-rows :before ((buffer completion-buffer-mode))
  (unless (require-match buffer)
    (insert-nodes (focus) (make-element "tr" :class "dummy-row"))))

(defmethod revert-buffer-aux :after ((buffer completion-buffer-mode))
  (setf (pos (focus))
        (or (npos-right-ensure
             (pos (focus))
             (lambda (p) (not (class-p p "dummy-row"))))
            (error 'top-of-subtree))))

(defmethod occur-p-aux :around ((buffer completion-buffer-mode)
                                (query t) element)
  (or (class-p element "dummy-row") (call-next-method)))

(defstyle completion-buffer-mode `(("dummy-row" :display "none")))

(defmethod selectable-p-aux ((buffer completion-buffer-mode) pos)
  (tag-name-p (node-after pos) "tr"))

(defmethod window-decoration-aux ((buffer minibuffer-completion-mode))
  (dom `((:div :class "buffer" :selectable "")
         ((:div :class "main content" :style "flex:0 0 2em;"
                :buffer ,(id buffer)))
         ((:div :class "content"
                :buffer ,(id (completion-buffer buffer)))))))

(defmethod on-delete-buffer progn ((buffer minibuffer-completion-mode))
  (delete-buffer (completion-buffer buffer)))

(define-command complete-minibuffer
  :mode minibuffer-completion-mode ()
  (complete-minibuffer-aux (current-buffer)))

(define-command complete-exit-minibuffer
  :mode minibuffer-completion-mode ()
  (complete-minibuffer)
  (exit-minibuffer))

(define-command next-completion
  :mode completion-mode ()
  (with-current-buffer (completion-buffer (current-buffer))
    (forward-element)))

(define-command previous-completion
  :mode completion-mode ()
  (with-current-buffer (completion-buffer (current-buffer))
    (backward-element)))

(define-command scroll-down-completion
  :mode completion-mode ()
  (with-current-buffer (completion-buffer (current-buffer))
    (dotimes (_ (scroll-lines (current-buffer)))
      (forward-element))))

(define-command scroll-up-completion
  :mode completion-mode ()
  (with-current-buffer (completion-buffer (current-buffer))
    (dotimes (_ (scroll-lines (current-buffer)))
      (backward-element))))

(define-command first-completion
  :mode completion-mode ()
  (with-current-buffer (completion-buffer (current-buffer))
    (beginning-of-buffer)))

(define-command last-completion
  :mode completion-mode ()
  (with-current-buffer (completion-buffer (current-buffer))
    (end-of-buffer)))

(defun make-completion-buffer (modes &rest args)
  (lret ((buffer (apply #'make-buffer "*completion*" :modes modes
                        args)))
    (with-current-buffer buffer
      (revert-buffer))))

(defun find-command (name modes)
  (iter (for mode in (append modes '(global)))
    (when-let
        (cmd (find name (commands mode)
                   :key (alex:compose #'string-downcase #'symbol-name)
                   :test 'equal))
      (return cmd))))

(defun collect-keybindings (keymaps)
  (lret (keys (table (make-hash-table)))
    (iter (for keymap in keymaps)
      (traverse-keymap
       keymap (lambda (kseq cmd)
                (declare (ignore cmd))
                (push kseq keys))))
    (iter (for k in (delete-duplicates keys :test 'equal))
      (push k (gethash (lookup-keybind k keymaps) table)))))

(define-command execute-command ()
  (let ((name (completing-read
               "M-x " 'command-list-mode
               :include-modes (modes (current-buffer))
               :keybinding-index
               (collect-keybindings (keymaps (current-buffer))))))
    (if-let (cmd (find-command name (modes (current-buffer))))
      (funcall cmd)
      (message "No such command"))))

(define-keys global
  "M-x" 'execute-command)

(defstyle minibuffer-prompt `(:inherit bold))

(defstyle minibuffer `((".prompt" :inherit minibuffer-prompt)))
